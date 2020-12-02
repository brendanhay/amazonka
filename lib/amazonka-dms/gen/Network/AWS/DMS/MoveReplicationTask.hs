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
-- Module      : Network.AWS.DMS.MoveReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves a replication task from its current replication instance to a different target replication instance using the specified parameters. The target replication instance must be created with the same or later AWS DMS version as the current replication instance.
module Network.AWS.DMS.MoveReplicationTask
  ( -- * Creating a Request
    moveReplicationTask,
    MoveReplicationTask,

    -- * Request Lenses
    mrtReplicationTaskARN,
    mrtTargetReplicationInstanceARN,

    -- * Destructuring the Response
    moveReplicationTaskResponse,
    MoveReplicationTaskResponse,

    -- * Response Lenses
    mrtrsReplicationTask,
    mrtrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'moveReplicationTask' smart constructor.
data MoveReplicationTask = MoveReplicationTask'
  { _mrtReplicationTaskARN ::
      !Text,
    _mrtTargetReplicationInstanceARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MoveReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrtReplicationTaskARN' - The Amazon Resource Name (ARN) of the task that you want to move.
--
-- * 'mrtTargetReplicationInstanceARN' - The ARN of the replication instance where you want to move the task to.
moveReplicationTask ::
  -- | 'mrtReplicationTaskARN'
  Text ->
  -- | 'mrtTargetReplicationInstanceARN'
  Text ->
  MoveReplicationTask
moveReplicationTask
  pReplicationTaskARN_
  pTargetReplicationInstanceARN_ =
    MoveReplicationTask'
      { _mrtReplicationTaskARN =
          pReplicationTaskARN_,
        _mrtTargetReplicationInstanceARN = pTargetReplicationInstanceARN_
      }

-- | The Amazon Resource Name (ARN) of the task that you want to move.
mrtReplicationTaskARN :: Lens' MoveReplicationTask Text
mrtReplicationTaskARN = lens _mrtReplicationTaskARN (\s a -> s {_mrtReplicationTaskARN = a})

-- | The ARN of the replication instance where you want to move the task to.
mrtTargetReplicationInstanceARN :: Lens' MoveReplicationTask Text
mrtTargetReplicationInstanceARN = lens _mrtTargetReplicationInstanceARN (\s a -> s {_mrtTargetReplicationInstanceARN = a})

instance AWSRequest MoveReplicationTask where
  type Rs MoveReplicationTask = MoveReplicationTaskResponse
  request = postJSON dms
  response =
    receiveJSON
      ( \s h x ->
          MoveReplicationTaskResponse'
            <$> (x .?> "ReplicationTask") <*> (pure (fromEnum s))
      )

instance Hashable MoveReplicationTask

instance NFData MoveReplicationTask

instance ToHeaders MoveReplicationTask where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonDMSv20160101.MoveReplicationTask" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON MoveReplicationTask where
  toJSON MoveReplicationTask' {..} =
    object
      ( catMaybes
          [ Just ("ReplicationTaskArn" .= _mrtReplicationTaskARN),
            Just
              ( "TargetReplicationInstanceArn"
                  .= _mrtTargetReplicationInstanceARN
              )
          ]
      )

instance ToPath MoveReplicationTask where
  toPath = const "/"

instance ToQuery MoveReplicationTask where
  toQuery = const mempty

-- |
--
--
--
-- /See:/ 'moveReplicationTaskResponse' smart constructor.
data MoveReplicationTaskResponse = MoveReplicationTaskResponse'
  { _mrtrsReplicationTask ::
      !(Maybe ReplicationTask),
    _mrtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MoveReplicationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrtrsReplicationTask' - The replication task that was moved.
--
-- * 'mrtrsResponseStatus' - -- | The response status code.
moveReplicationTaskResponse ::
  -- | 'mrtrsResponseStatus'
  Int ->
  MoveReplicationTaskResponse
moveReplicationTaskResponse pResponseStatus_ =
  MoveReplicationTaskResponse'
    { _mrtrsReplicationTask = Nothing,
      _mrtrsResponseStatus = pResponseStatus_
    }

-- | The replication task that was moved.
mrtrsReplicationTask :: Lens' MoveReplicationTaskResponse (Maybe ReplicationTask)
mrtrsReplicationTask = lens _mrtrsReplicationTask (\s a -> s {_mrtrsReplicationTask = a})

-- | -- | The response status code.
mrtrsResponseStatus :: Lens' MoveReplicationTaskResponse Int
mrtrsResponseStatus = lens _mrtrsResponseStatus (\s a -> s {_mrtrsResponseStatus = a})

instance NFData MoveReplicationTaskResponse
