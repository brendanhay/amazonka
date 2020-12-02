{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserBucketDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserBucketDetails where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon S3 bucket for the disk image.
--
--
--
-- /See:/ 'userBucketDetails' smart constructor.
data UserBucketDetails = UserBucketDetails'
  { _ubdS3Key ::
      !(Maybe Text),
    _ubdS3Bucket :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserBucketDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubdS3Key' - The file name of the disk image.
--
-- * 'ubdS3Bucket' - The Amazon S3 bucket from which the disk image was created.
userBucketDetails ::
  UserBucketDetails
userBucketDetails =
  UserBucketDetails' {_ubdS3Key = Nothing, _ubdS3Bucket = Nothing}

-- | The file name of the disk image.
ubdS3Key :: Lens' UserBucketDetails (Maybe Text)
ubdS3Key = lens _ubdS3Key (\s a -> s {_ubdS3Key = a})

-- | The Amazon S3 bucket from which the disk image was created.
ubdS3Bucket :: Lens' UserBucketDetails (Maybe Text)
ubdS3Bucket = lens _ubdS3Bucket (\s a -> s {_ubdS3Bucket = a})

instance FromXML UserBucketDetails where
  parseXML x =
    UserBucketDetails' <$> (x .@? "s3Key") <*> (x .@? "s3Bucket")

instance Hashable UserBucketDetails

instance NFData UserBucketDetails
