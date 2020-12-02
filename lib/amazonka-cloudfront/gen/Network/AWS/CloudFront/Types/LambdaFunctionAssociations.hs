{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.LambdaFunctionAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.LambdaFunctionAssociations where

import Network.AWS.CloudFront.Types.LambdaFunctionAssociation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that specifies a list of Lambda functions associations for a cache behavior.
--
--
-- If you want to invoke one or more Lambda functions triggered by requests that match the @PathPattern@ of the cache behavior, specify the applicable values for @Quantity@ and @Items@ . Note that there can be up to 4 @LambdaFunctionAssociation@ items in this list (one for each possible value of @EventType@ ) and each @EventType@ can be associated with the Lambda function only once.
--
-- If you don't want to invoke any Lambda functions for the requests that match @PathPattern@ , specify @0@ for @Quantity@ and omit @Items@ .
--
--
-- /See:/ 'lambdaFunctionAssociations' smart constructor.
data LambdaFunctionAssociations = LambdaFunctionAssociations'
  { _lfaItems ::
      !(Maybe [LambdaFunctionAssociation]),
    _lfaQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfaItems' - __Optional__ : A complex type that contains @LambdaFunctionAssociation@ items for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
--
-- * 'lfaQuantity' - The number of Lambda function associations for this cache behavior.
lambdaFunctionAssociations ::
  -- | 'lfaQuantity'
  Int ->
  LambdaFunctionAssociations
lambdaFunctionAssociations pQuantity_ =
  LambdaFunctionAssociations'
    { _lfaItems = Nothing,
      _lfaQuantity = pQuantity_
    }

-- | __Optional__ : A complex type that contains @LambdaFunctionAssociation@ items for this cache behavior. If @Quantity@ is @0@ , you can omit @Items@ .
lfaItems :: Lens' LambdaFunctionAssociations [LambdaFunctionAssociation]
lfaItems = lens _lfaItems (\s a -> s {_lfaItems = a}) . _Default . _Coerce

-- | The number of Lambda function associations for this cache behavior.
lfaQuantity :: Lens' LambdaFunctionAssociations Int
lfaQuantity = lens _lfaQuantity (\s a -> s {_lfaQuantity = a})

instance FromXML LambdaFunctionAssociations where
  parseXML x =
    LambdaFunctionAssociations'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "LambdaFunctionAssociation")
          )
      <*> (x .@ "Quantity")

instance Hashable LambdaFunctionAssociations

instance NFData LambdaFunctionAssociations

instance ToXML LambdaFunctionAssociations where
  toXML LambdaFunctionAssociations' {..} =
    mconcat
      [ "Items"
          @= toXML (toXMLList "LambdaFunctionAssociation" <$> _lfaItems),
        "Quantity" @= _lfaQuantity
      ]
