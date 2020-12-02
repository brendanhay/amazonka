{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginRequestPolicyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginRequestPolicyList where

import Network.AWS.CloudFront.Types.OriginRequestPolicySummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of origin request policies.
--
--
--
-- /See:/ 'originRequestPolicyList' smart constructor.
data OriginRequestPolicyList = OriginRequestPolicyList'
  { _orplItems ::
      !(Maybe [OriginRequestPolicySummary]),
    _orplNextMarker :: !(Maybe Text),
    _orplMaxItems :: !Int,
    _orplQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginRequestPolicyList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orplItems' - Contains the origin request policies in the list.
--
-- * 'orplNextMarker' - If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
--
-- * 'orplMaxItems' - The maximum number of origin request policies requested.
--
-- * 'orplQuantity' - The total number of origin request policies returned in the response.
originRequestPolicyList ::
  -- | 'orplMaxItems'
  Int ->
  -- | 'orplQuantity'
  Int ->
  OriginRequestPolicyList
originRequestPolicyList pMaxItems_ pQuantity_ =
  OriginRequestPolicyList'
    { _orplItems = Nothing,
      _orplNextMarker = Nothing,
      _orplMaxItems = pMaxItems_,
      _orplQuantity = pQuantity_
    }

-- | Contains the origin request policies in the list.
orplItems :: Lens' OriginRequestPolicyList [OriginRequestPolicySummary]
orplItems = lens _orplItems (\s a -> s {_orplItems = a}) . _Default . _Coerce

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing origin request policies where you left off.
orplNextMarker :: Lens' OriginRequestPolicyList (Maybe Text)
orplNextMarker = lens _orplNextMarker (\s a -> s {_orplNextMarker = a})

-- | The maximum number of origin request policies requested.
orplMaxItems :: Lens' OriginRequestPolicyList Int
orplMaxItems = lens _orplMaxItems (\s a -> s {_orplMaxItems = a})

-- | The total number of origin request policies returned in the response.
orplQuantity :: Lens' OriginRequestPolicyList Int
orplQuantity = lens _orplQuantity (\s a -> s {_orplQuantity = a})

instance FromXML OriginRequestPolicyList where
  parseXML x =
    OriginRequestPolicyList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "OriginRequestPolicySummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "Quantity")

instance Hashable OriginRequestPolicyList

instance NFData OriginRequestPolicyList
