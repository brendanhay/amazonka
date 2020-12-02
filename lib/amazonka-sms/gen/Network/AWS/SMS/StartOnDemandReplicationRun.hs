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
-- Module      : Network.AWS.SMS.StartOnDemandReplicationRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified replication job. This replication run starts immediately. This replication run is in addition to the ones already scheduled.
--
--
-- There is a limit on the number of on-demand replications runs that you can request in a 24-hour period.
module Network.AWS.SMS.StartOnDemandReplicationRun
  ( -- * Creating a Request
    startOnDemandReplicationRun,
    StartOnDemandReplicationRun,

    -- * Request Lenses
    sodrrDescription,
    sodrrReplicationJobId,

    -- * Destructuring the Response
    startOnDemandReplicationRunResponse,
    StartOnDemandReplicationRunResponse,

    -- * Response Lenses
    sodrrrsReplicationRunId,
    sodrrrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'startOnDemandReplicationRun' smart constructor.
data StartOnDemandReplicationRun = StartOnDemandReplicationRun'
  { _sodrrDescription ::
      !(Maybe Text),
    _sodrrReplicationJobId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartOnDemandReplicationRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sodrrDescription' - The description of the replication run.
--
-- * 'sodrrReplicationJobId' - The ID of the replication job.
startOnDemandReplicationRun ::
  -- | 'sodrrReplicationJobId'
  Text ->
  StartOnDemandReplicationRun
startOnDemandReplicationRun pReplicationJobId_ =
  StartOnDemandReplicationRun'
    { _sodrrDescription = Nothing,
      _sodrrReplicationJobId = pReplicationJobId_
    }

-- | The description of the replication run.
sodrrDescription :: Lens' StartOnDemandReplicationRun (Maybe Text)
sodrrDescription = lens _sodrrDescription (\s a -> s {_sodrrDescription = a})

-- | The ID of the replication job.
sodrrReplicationJobId :: Lens' StartOnDemandReplicationRun Text
sodrrReplicationJobId = lens _sodrrReplicationJobId (\s a -> s {_sodrrReplicationJobId = a})

instance AWSRequest StartOnDemandReplicationRun where
  type
    Rs StartOnDemandReplicationRun =
      StartOnDemandReplicationRunResponse
  request = postJSON sms
  response =
    receiveJSON
      ( \s h x ->
          StartOnDemandReplicationRunResponse'
            <$> (x .?> "replicationRunId") <*> (pure (fromEnum s))
      )

instance Hashable StartOnDemandReplicationRun

instance NFData StartOnDemandReplicationRun

instance ToHeaders StartOnDemandReplicationRun where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.StartOnDemandReplicationRun" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartOnDemandReplicationRun where
  toJSON StartOnDemandReplicationRun' {..} =
    object
      ( catMaybes
          [ ("description" .=) <$> _sodrrDescription,
            Just ("replicationJobId" .= _sodrrReplicationJobId)
          ]
      )

instance ToPath StartOnDemandReplicationRun where
  toPath = const "/"

instance ToQuery StartOnDemandReplicationRun where
  toQuery = const mempty

-- | /See:/ 'startOnDemandReplicationRunResponse' smart constructor.
data StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse'
  { _sodrrrsReplicationRunId ::
      !(Maybe Text),
    _sodrrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartOnDemandReplicationRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sodrrrsReplicationRunId' - The ID of the replication run.
--
-- * 'sodrrrsResponseStatus' - -- | The response status code.
startOnDemandReplicationRunResponse ::
  -- | 'sodrrrsResponseStatus'
  Int ->
  StartOnDemandReplicationRunResponse
startOnDemandReplicationRunResponse pResponseStatus_ =
  StartOnDemandReplicationRunResponse'
    { _sodrrrsReplicationRunId =
        Nothing,
      _sodrrrsResponseStatus = pResponseStatus_
    }

-- | The ID of the replication run.
sodrrrsReplicationRunId :: Lens' StartOnDemandReplicationRunResponse (Maybe Text)
sodrrrsReplicationRunId = lens _sodrrrsReplicationRunId (\s a -> s {_sodrrrsReplicationRunId = a})

-- | -- | The response status code.
sodrrrsResponseStatus :: Lens' StartOnDemandReplicationRunResponse Int
sodrrrsResponseStatus = lens _sodrrrsResponseStatus (\s a -> s {_sodrrrsResponseStatus = a})

instance NFData StartOnDemandReplicationRunResponse
