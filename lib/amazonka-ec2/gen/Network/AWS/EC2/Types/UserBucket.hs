{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserBucket where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon S3 bucket for the disk image.
--
--
--
-- /See:/ 'userBucket' smart constructor.
data UserBucket = UserBucket'
  { _ubS3Key :: !(Maybe Text),
    _ubS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserBucket' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubS3Key' - The file name of the disk image.
--
-- * 'ubS3Bucket' - The name of the Amazon S3 bucket where the disk image is located.
userBucket ::
  UserBucket
userBucket = UserBucket' {_ubS3Key = Nothing, _ubS3Bucket = Nothing}

-- | The file name of the disk image.
ubS3Key :: Lens' UserBucket (Maybe Text)
ubS3Key = lens _ubS3Key (\s a -> s {_ubS3Key = a})

-- | The name of the Amazon S3 bucket where the disk image is located.
ubS3Bucket :: Lens' UserBucket (Maybe Text)
ubS3Bucket = lens _ubS3Bucket (\s a -> s {_ubS3Bucket = a})

instance Hashable UserBucket

instance NFData UserBucket

instance ToQuery UserBucket where
  toQuery UserBucket' {..} =
    mconcat ["S3Key" =: _ubS3Key, "S3Bucket" =: _ubS3Bucket]
