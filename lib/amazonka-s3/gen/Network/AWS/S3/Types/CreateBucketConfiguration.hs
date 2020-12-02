{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CreateBucketConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CreateBucketConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | The configuration information for the bucket.
--
--
--
-- /See:/ 'createBucketConfiguration' smart constructor.
newtype CreateBucketConfiguration = CreateBucketConfiguration'
  { _cbcLocationConstraint ::
      Maybe LocationConstraint
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateBucketConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbcLocationConstraint' - Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
createBucketConfiguration ::
  CreateBucketConfiguration
createBucketConfiguration =
  CreateBucketConfiguration' {_cbcLocationConstraint = Nothing}

-- | Specifies the Region where the bucket will be created. If you don't specify a Region, the bucket is created in the US East (N. Virginia) Region (us-east-1).
cbcLocationConstraint :: Lens' CreateBucketConfiguration (Maybe LocationConstraint)
cbcLocationConstraint = lens _cbcLocationConstraint (\s a -> s {_cbcLocationConstraint = a})

instance Hashable CreateBucketConfiguration

instance NFData CreateBucketConfiguration

instance ToXML CreateBucketConfiguration where
  toXML CreateBucketConfiguration' {..} =
    mconcat ["LocationConstraint" @= _cbcLocationConstraint]
