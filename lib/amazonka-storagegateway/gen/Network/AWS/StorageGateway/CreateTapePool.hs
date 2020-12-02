{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom tape pool. You can use custom tape pool to enable tape retention lock on tapes that are archived in the custom pool.
module Network.AWS.StorageGateway.CreateTapePool
  ( -- * Creating a Request
    createTapePool,
    CreateTapePool,

    -- * Request Lenses
    ctpRetentionLockType,
    ctpRetentionLockTimeInDays,
    ctpTags,
    ctpPoolName,
    ctpStorageClass,

    -- * Destructuring the Response
    createTapePoolResponse,
    CreateTapePoolResponse,

    -- * Response Lenses
    ctprsPoolARN,
    ctprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'createTapePool' smart constructor.
data CreateTapePool = CreateTapePool'
  { _ctpRetentionLockType ::
      !(Maybe RetentionLockType),
    _ctpRetentionLockTimeInDays :: !(Maybe Nat),
    _ctpTags :: !(Maybe [Tag]),
    _ctpPoolName :: !Text,
    _ctpStorageClass :: !TapeStorageClass
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTapePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctpRetentionLockType' - Tape retention lock can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
--
-- * 'ctpRetentionLockTimeInDays' - Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
--
-- * 'ctpTags' - A list of up to 50 tags that can be assigned to tape pool. Each tag is a key-value pair.
--
-- * 'ctpPoolName' - The name of the new custom tape pool.
--
-- * 'ctpStorageClass' - The storage class that is associated with the new custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
createTapePool ::
  -- | 'ctpPoolName'
  Text ->
  -- | 'ctpStorageClass'
  TapeStorageClass ->
  CreateTapePool
createTapePool pPoolName_ pStorageClass_ =
  CreateTapePool'
    { _ctpRetentionLockType = Nothing,
      _ctpRetentionLockTimeInDays = Nothing,
      _ctpTags = Nothing,
      _ctpPoolName = pPoolName_,
      _ctpStorageClass = pStorageClass_
    }

-- | Tape retention lock can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
ctpRetentionLockType :: Lens' CreateTapePool (Maybe RetentionLockType)
ctpRetentionLockType = lens _ctpRetentionLockType (\s a -> s {_ctpRetentionLockType = a})

-- | Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
ctpRetentionLockTimeInDays :: Lens' CreateTapePool (Maybe Natural)
ctpRetentionLockTimeInDays = lens _ctpRetentionLockTimeInDays (\s a -> s {_ctpRetentionLockTimeInDays = a}) . mapping _Nat

-- | A list of up to 50 tags that can be assigned to tape pool. Each tag is a key-value pair.
ctpTags :: Lens' CreateTapePool [Tag]
ctpTags = lens _ctpTags (\s a -> s {_ctpTags = a}) . _Default . _Coerce

-- | The name of the new custom tape pool.
ctpPoolName :: Lens' CreateTapePool Text
ctpPoolName = lens _ctpPoolName (\s a -> s {_ctpPoolName = a})

-- | The storage class that is associated with the new custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
ctpStorageClass :: Lens' CreateTapePool TapeStorageClass
ctpStorageClass = lens _ctpStorageClass (\s a -> s {_ctpStorageClass = a})

instance AWSRequest CreateTapePool where
  type Rs CreateTapePool = CreateTapePoolResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          CreateTapePoolResponse'
            <$> (x .?> "PoolARN") <*> (pure (fromEnum s))
      )

instance Hashable CreateTapePool

instance NFData CreateTapePool

instance ToHeaders CreateTapePool where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.CreateTapePool" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateTapePool where
  toJSON CreateTapePool' {..} =
    object
      ( catMaybes
          [ ("RetentionLockType" .=) <$> _ctpRetentionLockType,
            ("RetentionLockTimeInDays" .=) <$> _ctpRetentionLockTimeInDays,
            ("Tags" .=) <$> _ctpTags,
            Just ("PoolName" .= _ctpPoolName),
            Just ("StorageClass" .= _ctpStorageClass)
          ]
      )

instance ToPath CreateTapePool where
  toPath = const "/"

instance ToQuery CreateTapePool where
  toQuery = const mempty

-- | /See:/ 'createTapePoolResponse' smart constructor.
data CreateTapePoolResponse = CreateTapePoolResponse'
  { _ctprsPoolARN ::
      !(Maybe Text),
    _ctprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateTapePoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctprsPoolARN' - The unique Amazon Resource Name (ARN) that represents the custom tape pool. Use the 'ListTapePools' operation to return a list of tape pools for your account and AWS Region.
--
-- * 'ctprsResponseStatus' - -- | The response status code.
createTapePoolResponse ::
  -- | 'ctprsResponseStatus'
  Int ->
  CreateTapePoolResponse
createTapePoolResponse pResponseStatus_ =
  CreateTapePoolResponse'
    { _ctprsPoolARN = Nothing,
      _ctprsResponseStatus = pResponseStatus_
    }

-- | The unique Amazon Resource Name (ARN) that represents the custom tape pool. Use the 'ListTapePools' operation to return a list of tape pools for your account and AWS Region.
ctprsPoolARN :: Lens' CreateTapePoolResponse (Maybe Text)
ctprsPoolARN = lens _ctprsPoolARN (\s a -> s {_ctprsPoolARN = a})

-- | -- | The response status code.
ctprsResponseStatus :: Lens' CreateTapePoolResponse Int
ctprsResponseStatus = lens _ctprsResponseStatus (\s a -> s {_ctprsResponseStatus = a})

instance NFData CreateTapePoolResponse
