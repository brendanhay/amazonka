{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.CachePolicyList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.CachePolicyList where

import Network.AWS.CloudFront.Types.CachePolicySummary
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of cache policies.
--
--
--
-- /See:/ 'cachePolicyList' smart constructor.
data CachePolicyList = CachePolicyList'
  { _cplItems ::
      !(Maybe [CachePolicySummary]),
    _cplNextMarker :: !(Maybe Text),
    _cplMaxItems :: !Int,
    _cplQuantity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CachePolicyList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cplItems' - Contains the cache policies in the list.
--
-- * 'cplNextMarker' - If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing cache policies where you left off.
--
-- * 'cplMaxItems' - The maximum number of cache policies requested.
--
-- * 'cplQuantity' - The total number of cache policies returned in the response.
cachePolicyList ::
  -- | 'cplMaxItems'
  Int ->
  -- | 'cplQuantity'
  Int ->
  CachePolicyList
cachePolicyList pMaxItems_ pQuantity_ =
  CachePolicyList'
    { _cplItems = Nothing,
      _cplNextMarker = Nothing,
      _cplMaxItems = pMaxItems_,
      _cplQuantity = pQuantity_
    }

-- | Contains the cache policies in the list.
cplItems :: Lens' CachePolicyList [CachePolicySummary]
cplItems = lens _cplItems (\s a -> s {_cplItems = a}) . _Default . _Coerce

-- | If there are more items in the list than are in this response, this element is present. It contains the value that you should use in the @Marker@ field of a subsequent request to continue listing cache policies where you left off.
cplNextMarker :: Lens' CachePolicyList (Maybe Text)
cplNextMarker = lens _cplNextMarker (\s a -> s {_cplNextMarker = a})

-- | The maximum number of cache policies requested.
cplMaxItems :: Lens' CachePolicyList Int
cplMaxItems = lens _cplMaxItems (\s a -> s {_cplMaxItems = a})

-- | The total number of cache policies returned in the response.
cplQuantity :: Lens' CachePolicyList Int
cplQuantity = lens _cplQuantity (\s a -> s {_cplQuantity = a})

instance FromXML CachePolicyList where
  parseXML x =
    CachePolicyList'
      <$> ( x .@? "Items" .!@ mempty
              >>= may (parseXMLList "CachePolicySummary")
          )
      <*> (x .@? "NextMarker")
      <*> (x .@ "MaxItems")
      <*> (x .@ "Quantity")

instance Hashable CachePolicyList

instance NFData CachePolicyList
