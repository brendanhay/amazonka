{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessKeyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessKeyInfo where

import Network.AWS.IAM.Types.StatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an AWS access key.
--
--
-- This data type is used as a response element in the 'CreateAccessKey' and 'ListAccessKeys' operations.
--
--
-- /See:/ 'accessKeyInfo' smart constructor.
data AccessKeyInfo = AccessKeyInfo'
  { _akiCreateDate ::
      !(Maybe ISO8601),
    _akiUserName :: !Text,
    _akiAccessKeyId :: !AccessKey,
    _akiStatus :: !StatusType,
    _akiSecretAccessKey :: !(Sensitive Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessKeyInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akiCreateDate' - The date when the access key was created.
--
-- * 'akiUserName' - The name of the IAM user that the access key is associated with.
--
-- * 'akiAccessKeyId' - The ID for this access key.
--
-- * 'akiStatus' - The status of the access key. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
--
-- * 'akiSecretAccessKey' - The secret key used to sign requests.
accessKeyInfo ::
  -- | 'akiUserName'
  Text ->
  -- | 'akiAccessKeyId'
  AccessKey ->
  -- | 'akiStatus'
  StatusType ->
  -- | 'akiSecretAccessKey'
  Text ->
  AccessKeyInfo
accessKeyInfo pUserName_ pAccessKeyId_ pStatus_ pSecretAccessKey_ =
  AccessKeyInfo'
    { _akiCreateDate = Nothing,
      _akiUserName = pUserName_,
      _akiAccessKeyId = pAccessKeyId_,
      _akiStatus = pStatus_,
      _akiSecretAccessKey = _Sensitive # pSecretAccessKey_
    }

-- | The date when the access key was created.
akiCreateDate :: Lens' AccessKeyInfo (Maybe UTCTime)
akiCreateDate = lens _akiCreateDate (\s a -> s {_akiCreateDate = a}) . mapping _Time

-- | The name of the IAM user that the access key is associated with.
akiUserName :: Lens' AccessKeyInfo Text
akiUserName = lens _akiUserName (\s a -> s {_akiUserName = a})

-- | The ID for this access key.
akiAccessKeyId :: Lens' AccessKeyInfo AccessKey
akiAccessKeyId = lens _akiAccessKeyId (\s a -> s {_akiAccessKeyId = a})

-- | The status of the access key. @Active@ means that the key is valid for API calls, while @Inactive@ means it is not.
akiStatus :: Lens' AccessKeyInfo StatusType
akiStatus = lens _akiStatus (\s a -> s {_akiStatus = a})

-- | The secret key used to sign requests.
akiSecretAccessKey :: Lens' AccessKeyInfo Text
akiSecretAccessKey = lens _akiSecretAccessKey (\s a -> s {_akiSecretAccessKey = a}) . _Sensitive

instance FromXML AccessKeyInfo where
  parseXML x =
    AccessKeyInfo'
      <$> (x .@? "CreateDate")
      <*> (x .@ "UserName")
      <*> (x .@ "AccessKeyId")
      <*> (x .@ "Status")
      <*> (x .@ "SecretAccessKey")

instance Hashable AccessKeyInfo

instance NFData AccessKeyInfo
