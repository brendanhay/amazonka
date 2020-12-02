{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.BucketInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.BucketInfo where

import Network.AWS.CloudSearchDomains.Types.Bucket
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for the calculated facet values and counts.
--
--
--
-- /See:/ 'bucketInfo' smart constructor.
newtype BucketInfo = BucketInfo' {_biBuckets :: Maybe [Bucket]}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BucketInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'biBuckets' - A list of the calculated facet values and counts.
bucketInfo ::
  BucketInfo
bucketInfo = BucketInfo' {_biBuckets = Nothing}

-- | A list of the calculated facet values and counts.
biBuckets :: Lens' BucketInfo [Bucket]
biBuckets = lens _biBuckets (\s a -> s {_biBuckets = a}) . _Default . _Coerce

instance FromJSON BucketInfo where
  parseJSON =
    withObject
      "BucketInfo"
      (\x -> BucketInfo' <$> (x .:? "buckets" .!= mempty))

instance Hashable BucketInfo

instance NFData BucketInfo
