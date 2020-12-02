{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.BucketLifecycleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketLifecycleConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LifecycleRule

-- | Specifies the lifecycle configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'bucketLifecycleConfiguration' smart constructor.
newtype BucketLifecycleConfiguration = BucketLifecycleConfiguration'
  { _blcRules ::
      [LifecycleRule]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BucketLifecycleConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blcRules' - A lifecycle rule for individual objects in an Amazon S3 bucket.
bucketLifecycleConfiguration ::
  BucketLifecycleConfiguration
bucketLifecycleConfiguration =
  BucketLifecycleConfiguration' {_blcRules = mempty}

-- | A lifecycle rule for individual objects in an Amazon S3 bucket.
blcRules :: Lens' BucketLifecycleConfiguration [LifecycleRule]
blcRules = lens _blcRules (\s a -> s {_blcRules = a}) . _Coerce

instance Hashable BucketLifecycleConfiguration

instance NFData BucketLifecycleConfiguration

instance ToXML BucketLifecycleConfiguration where
  toXML BucketLifecycleConfiguration' {..} =
    mconcat [toXMLList "Rule" _blcRules]
