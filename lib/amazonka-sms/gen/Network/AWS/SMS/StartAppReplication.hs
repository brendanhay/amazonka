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
-- Module      : Network.AWS.SMS.StartAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts replicating the specified application by creating replication jobs for each server in the application.
module Network.AWS.SMS.StartAppReplication
  ( -- * Creating a Request
    startAppReplication,
    StartAppReplication,

    -- * Request Lenses
    sarAppId,

    -- * Destructuring the Response
    startAppReplicationResponse,
    StartAppReplicationResponse,

    -- * Response Lenses
    srsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'startAppReplication' smart constructor.
newtype StartAppReplication = StartAppReplication'
  { _sarAppId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartAppReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sarAppId' - The ID of the application.
startAppReplication ::
  StartAppReplication
startAppReplication = StartAppReplication' {_sarAppId = Nothing}

-- | The ID of the application.
sarAppId :: Lens' StartAppReplication (Maybe Text)
sarAppId = lens _sarAppId (\s a -> s {_sarAppId = a})

instance AWSRequest StartAppReplication where
  type Rs StartAppReplication = StartAppReplicationResponse
  request = postJSON sms
  response =
    receiveEmpty
      (\s h x -> StartAppReplicationResponse' <$> (pure (fromEnum s)))

instance Hashable StartAppReplication

instance NFData StartAppReplication

instance ToHeaders StartAppReplication where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.StartAppReplication" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartAppReplication where
  toJSON StartAppReplication' {..} =
    object (catMaybes [("appId" .=) <$> _sarAppId])

instance ToPath StartAppReplication where
  toPath = const "/"

instance ToQuery StartAppReplication where
  toQuery = const mempty

-- | /See:/ 'startAppReplicationResponse' smart constructor.
newtype StartAppReplicationResponse = StartAppReplicationResponse'
  { _srsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartAppReplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
startAppReplicationResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StartAppReplicationResponse
startAppReplicationResponse pResponseStatus_ =
  StartAppReplicationResponse'
    { _srsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
srsResponseStatus :: Lens' StartAppReplicationResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData StartAppReplicationResponse
