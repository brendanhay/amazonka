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
-- Module      : Network.AWS.StorageGateway.AssignTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns a tape to a tape pool for archiving. The tape assigned to a pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the S3 storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
module Network.AWS.StorageGateway.AssignTapePool
  ( -- * Creating a Request
    assignTapePool,
    AssignTapePool,

    -- * Request Lenses
    atpBypassGovernanceRetention,
    atpTapeARN,
    atpPoolId,

    -- * Destructuring the Response
    assignTapePoolResponse,
    AssignTapePoolResponse,

    -- * Response Lenses
    atprsTapeARN,
    atprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'assignTapePool' smart constructor.
data AssignTapePool = AssignTapePool'
  { _atpBypassGovernanceRetention ::
      !(Maybe Bool),
    _atpTapeARN :: !Text,
    _atpPoolId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssignTapePool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atpBypassGovernanceRetention' - Set permissions to bypass governance retention. If the lock type of the archived tape is @Governance@ , the tape's archived age is not older than @RetentionLockInDays@ , and the user does not already have @BypassGovernanceRetention@ , setting this to TRUE enables the user to bypass the retention lock. This parameter is set to true by default for calls from the console. Valid values: @TRUE@ | @FALSE@
--
-- * 'atpTapeARN' - The unique Amazon Resource Name (ARN) of the virtual tape that you want to add to the tape pool.
--
-- * 'atpPoolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool. Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
assignTapePool ::
  -- | 'atpTapeARN'
  Text ->
  -- | 'atpPoolId'
  Text ->
  AssignTapePool
assignTapePool pTapeARN_ pPoolId_ =
  AssignTapePool'
    { _atpBypassGovernanceRetention = Nothing,
      _atpTapeARN = pTapeARN_,
      _atpPoolId = pPoolId_
    }

-- | Set permissions to bypass governance retention. If the lock type of the archived tape is @Governance@ , the tape's archived age is not older than @RetentionLockInDays@ , and the user does not already have @BypassGovernanceRetention@ , setting this to TRUE enables the user to bypass the retention lock. This parameter is set to true by default for calls from the console. Valid values: @TRUE@ | @FALSE@
atpBypassGovernanceRetention :: Lens' AssignTapePool (Maybe Bool)
atpBypassGovernanceRetention = lens _atpBypassGovernanceRetention (\s a -> s {_atpBypassGovernanceRetention = a})

-- | The unique Amazon Resource Name (ARN) of the virtual tape that you want to add to the tape pool.
atpTapeARN :: Lens' AssignTapePool Text
atpTapeARN = lens _atpTapeARN (\s a -> s {_atpTapeARN = a})

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool. Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
atpPoolId :: Lens' AssignTapePool Text
atpPoolId = lens _atpPoolId (\s a -> s {_atpPoolId = a})

instance AWSRequest AssignTapePool where
  type Rs AssignTapePool = AssignTapePoolResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          AssignTapePoolResponse'
            <$> (x .?> "TapeARN") <*> (pure (fromEnum s))
      )

instance Hashable AssignTapePool

instance NFData AssignTapePool

instance ToHeaders AssignTapePool where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.AssignTapePool" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AssignTapePool where
  toJSON AssignTapePool' {..} =
    object
      ( catMaybes
          [ ("BypassGovernanceRetention" .=)
              <$> _atpBypassGovernanceRetention,
            Just ("TapeARN" .= _atpTapeARN),
            Just ("PoolId" .= _atpPoolId)
          ]
      )

instance ToPath AssignTapePool where
  toPath = const "/"

instance ToQuery AssignTapePool where
  toQuery = const mempty

-- | /See:/ 'assignTapePoolResponse' smart constructor.
data AssignTapePoolResponse = AssignTapePoolResponse'
  { _atprsTapeARN ::
      !(Maybe Text),
    _atprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssignTapePoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atprsTapeARN' - The unique Amazon Resource Names (ARN) of the virtual tape that was added to the tape pool.
--
-- * 'atprsResponseStatus' - -- | The response status code.
assignTapePoolResponse ::
  -- | 'atprsResponseStatus'
  Int ->
  AssignTapePoolResponse
assignTapePoolResponse pResponseStatus_ =
  AssignTapePoolResponse'
    { _atprsTapeARN = Nothing,
      _atprsResponseStatus = pResponseStatus_
    }

-- | The unique Amazon Resource Names (ARN) of the virtual tape that was added to the tape pool.
atprsTapeARN :: Lens' AssignTapePoolResponse (Maybe Text)
atprsTapeARN = lens _atprsTapeARN (\s a -> s {_atprsTapeARN = a})

-- | -- | The response status code.
atprsResponseStatus :: Lens' AssignTapePoolResponse Int
atprsResponseStatus = lens _atprsResponseStatus (\s a -> s {_atprsResponseStatus = a})

instance NFData AssignTapePoolResponse
