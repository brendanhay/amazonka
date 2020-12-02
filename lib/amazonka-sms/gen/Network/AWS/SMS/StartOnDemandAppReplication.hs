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
-- Module      : Network.AWS.SMS.StartOnDemandAppReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an on-demand replication run for the specified application.
module Network.AWS.SMS.StartOnDemandAppReplication
  ( -- * Creating a Request
    startOnDemandAppReplication,
    StartOnDemandAppReplication,

    -- * Request Lenses
    sodarDescription,
    sodarAppId,

    -- * Destructuring the Response
    startOnDemandAppReplicationResponse,
    StartOnDemandAppReplicationResponse,

    -- * Response Lenses
    sodarrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'startOnDemandAppReplication' smart constructor.
data StartOnDemandAppReplication = StartOnDemandAppReplication'
  { _sodarDescription ::
      !(Maybe Text),
    _sodarAppId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartOnDemandAppReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sodarDescription' - The description of the replication run.
--
-- * 'sodarAppId' - The ID of the application.
startOnDemandAppReplication ::
  -- | 'sodarAppId'
  Text ->
  StartOnDemandAppReplication
startOnDemandAppReplication pAppId_ =
  StartOnDemandAppReplication'
    { _sodarDescription = Nothing,
      _sodarAppId = pAppId_
    }

-- | The description of the replication run.
sodarDescription :: Lens' StartOnDemandAppReplication (Maybe Text)
sodarDescription = lens _sodarDescription (\s a -> s {_sodarDescription = a})

-- | The ID of the application.
sodarAppId :: Lens' StartOnDemandAppReplication Text
sodarAppId = lens _sodarAppId (\s a -> s {_sodarAppId = a})

instance AWSRequest StartOnDemandAppReplication where
  type
    Rs StartOnDemandAppReplication =
      StartOnDemandAppReplicationResponse
  request = postJSON sms
  response =
    receiveEmpty
      ( \s h x ->
          StartOnDemandAppReplicationResponse' <$> (pure (fromEnum s))
      )

instance Hashable StartOnDemandAppReplication

instance NFData StartOnDemandAppReplication

instance ToHeaders StartOnDemandAppReplication where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.StartOnDemandAppReplication" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartOnDemandAppReplication where
  toJSON StartOnDemandAppReplication' {..} =
    object
      ( catMaybes
          [ ("description" .=) <$> _sodarDescription,
            Just ("appId" .= _sodarAppId)
          ]
      )

instance ToPath StartOnDemandAppReplication where
  toPath = const "/"

instance ToQuery StartOnDemandAppReplication where
  toQuery = const mempty

-- | /See:/ 'startOnDemandAppReplicationResponse' smart constructor.
newtype StartOnDemandAppReplicationResponse = StartOnDemandAppReplicationResponse'
  { _sodarrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartOnDemandAppReplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sodarrsResponseStatus' - -- | The response status code.
startOnDemandAppReplicationResponse ::
  -- | 'sodarrsResponseStatus'
  Int ->
  StartOnDemandAppReplicationResponse
startOnDemandAppReplicationResponse pResponseStatus_ =
  StartOnDemandAppReplicationResponse'
    { _sodarrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
sodarrsResponseStatus :: Lens' StartOnDemandAppReplicationResponse Int
sodarrsResponseStatus = lens _sodarrsResponseStatus (\s a -> s {_sodarrsResponseStatus = a})

instance NFData StartOnDemandAppReplicationResponse
