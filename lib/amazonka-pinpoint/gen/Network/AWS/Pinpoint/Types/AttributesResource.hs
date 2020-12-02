{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AttributesResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AttributesResource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about the type and the names of attributes that were removed from all the endpoints that are associated with an application.
--
--
--
-- /See:/ 'attributesResource' smart constructor.
data AttributesResource = AttributesResource'
  { _arAttributes ::
      !(Maybe [Text]),
    _arAttributeType :: !Text,
    _arApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttributesResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arAttributes' - An array that specifies the names of the attributes that were removed from the endpoints.
--
-- * 'arAttributeType' - The type of attribute or attributes that were removed from the endpoints. Valid values are:     * endpoint-custom-attributes - Custom attributes that describe endpoints.     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints.     * endpoint-user-attributes - Custom attributes that describe users.
--
-- * 'arApplicationId' - The unique identifier for the application.
attributesResource ::
  -- | 'arAttributeType'
  Text ->
  -- | 'arApplicationId'
  Text ->
  AttributesResource
attributesResource pAttributeType_ pApplicationId_ =
  AttributesResource'
    { _arAttributes = Nothing,
      _arAttributeType = pAttributeType_,
      _arApplicationId = pApplicationId_
    }

-- | An array that specifies the names of the attributes that were removed from the endpoints.
arAttributes :: Lens' AttributesResource [Text]
arAttributes = lens _arAttributes (\s a -> s {_arAttributes = a}) . _Default . _Coerce

-- | The type of attribute or attributes that were removed from the endpoints. Valid values are:     * endpoint-custom-attributes - Custom attributes that describe endpoints.     * endpoint-metric-attributes - Custom metrics that your app reports to Amazon Pinpoint for endpoints.     * endpoint-user-attributes - Custom attributes that describe users.
arAttributeType :: Lens' AttributesResource Text
arAttributeType = lens _arAttributeType (\s a -> s {_arAttributeType = a})

-- | The unique identifier for the application.
arApplicationId :: Lens' AttributesResource Text
arApplicationId = lens _arApplicationId (\s a -> s {_arApplicationId = a})

instance FromJSON AttributesResource where
  parseJSON =
    withObject
      "AttributesResource"
      ( \x ->
          AttributesResource'
            <$> (x .:? "Attributes" .!= mempty)
            <*> (x .: "AttributeType")
            <*> (x .: "ApplicationId")
      )

instance Hashable AttributesResource

instance NFData AttributesResource
