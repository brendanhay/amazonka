{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.S3Action
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Action where

import Network.AWS.IoT.Types.CannedAccessControlList
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to write data to an Amazon S3 bucket.
--
--
--
-- /See:/ 's3Action' smart constructor.
data S3Action = S3Action'
  { _sCannedACL ::
      !(Maybe CannedAccessControlList),
    _sRoleARN :: !Text,
    _sBucketName :: !Text,
    _sKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Action' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sCannedACL' - The Amazon S3 canned ACL that controls access to the object identified by the object key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs> .
--
-- * 'sRoleARN' - The ARN of the IAM role that grants access.
--
-- * 'sBucketName' - The Amazon S3 bucket.
--
-- * 'sKey' - The object key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, resources, and condition keys for Amazon S3> .
s3Action ::
  -- | 'sRoleARN'
  Text ->
  -- | 'sBucketName'
  Text ->
  -- | 'sKey'
  Text ->
  S3Action
s3Action pRoleARN_ pBucketName_ pKey_ =
  S3Action'
    { _sCannedACL = Nothing,
      _sRoleARN = pRoleARN_,
      _sBucketName = pBucketName_,
      _sKey = pKey_
    }

-- | The Amazon S3 canned ACL that controls access to the object identified by the object key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl S3 canned ACLs> .
sCannedACL :: Lens' S3Action (Maybe CannedAccessControlList)
sCannedACL = lens _sCannedACL (\s a -> s {_sCannedACL = a})

-- | The ARN of the IAM role that grants access.
sRoleARN :: Lens' S3Action Text
sRoleARN = lens _sRoleARN (\s a -> s {_sRoleARN = a})

-- | The Amazon S3 bucket.
sBucketName :: Lens' S3Action Text
sBucketName = lens _sBucketName (\s a -> s {_sBucketName = a})

-- | The object key. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/list_amazons3.html Actions, resources, and condition keys for Amazon S3> .
sKey :: Lens' S3Action Text
sKey = lens _sKey (\s a -> s {_sKey = a})

instance FromJSON S3Action where
  parseJSON =
    withObject
      "S3Action"
      ( \x ->
          S3Action'
            <$> (x .:? "cannedAcl")
            <*> (x .: "roleArn")
            <*> (x .: "bucketName")
            <*> (x .: "key")
      )

instance Hashable S3Action

instance NFData S3Action

instance ToJSON S3Action where
  toJSON S3Action' {..} =
    object
      ( catMaybes
          [ ("cannedAcl" .=) <$> _sCannedACL,
            Just ("roleArn" .= _sRoleARN),
            Just ("bucketName" .= _sBucketName),
            Just ("key" .= _sKey)
          ]
      )
