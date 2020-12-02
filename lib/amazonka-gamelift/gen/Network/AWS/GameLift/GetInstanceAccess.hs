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
-- Module      : Network.AWS.GameLift.GetInstanceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests remote access to a fleet instance. Remote access is useful for debugging, gathering benchmarking data, or observing activity in real time.
--
--
-- To remotely access an instance, you need credentials that match the operating system of the instance. For a Windows instance, Amazon GameLift returns a user name and password as strings for use with a Windows Remote Desktop client. For a Linux instance, Amazon GameLift returns a user name and RSA private key, also as strings, for use with an SSH client. The private key must be saved in the proper format to a @.pem@ file before using. If you're making this request using the AWS CLI, saving the secret can be handled as part of the GetInstanceAccess request, as shown in one of the examples for this operation.
--
-- To request access to a specific instance, specify the IDs of both the instance and the fleet it belongs to. You can retrieve a fleet's instance IDs by calling 'DescribeInstances' . If successful, an 'InstanceAccess' object is returned that contains the instance's IP address and a set of credentials.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances>
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues>
--
-- __Related operations__
--
--     * 'DescribeInstances'
--
--     * 'GetInstanceAccess'
module Network.AWS.GameLift.GetInstanceAccess
  ( -- * Creating a Request
    getInstanceAccess,
    GetInstanceAccess,

    -- * Request Lenses
    giaFleetId,
    giaInstanceId,

    -- * Destructuring the Response
    getInstanceAccessResponse,
    GetInstanceAccessResponse,

    -- * Response Lenses
    giarsInstanceAccess,
    giarsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'getInstanceAccess' smart constructor.
data GetInstanceAccess = GetInstanceAccess'
  { _giaFleetId :: !Text,
    _giaInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInstanceAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giaFleetId' - A unique identifier for a fleet that contains the instance you want access to. You can use either the fleet ID or ARN value. The fleet can be in any of the following statuses: @ACTIVATING@ , @ACTIVE@ , or @ERROR@ . Fleets with an @ERROR@ status may be accessible for a short time before they are deleted.
--
-- * 'giaInstanceId' - A unique identifier for an instance you want to get access to. You can access an instance in any status.
getInstanceAccess ::
  -- | 'giaFleetId'
  Text ->
  -- | 'giaInstanceId'
  Text ->
  GetInstanceAccess
getInstanceAccess pFleetId_ pInstanceId_ =
  GetInstanceAccess'
    { _giaFleetId = pFleetId_,
      _giaInstanceId = pInstanceId_
    }

-- | A unique identifier for a fleet that contains the instance you want access to. You can use either the fleet ID or ARN value. The fleet can be in any of the following statuses: @ACTIVATING@ , @ACTIVE@ , or @ERROR@ . Fleets with an @ERROR@ status may be accessible for a short time before they are deleted.
giaFleetId :: Lens' GetInstanceAccess Text
giaFleetId = lens _giaFleetId (\s a -> s {_giaFleetId = a})

-- | A unique identifier for an instance you want to get access to. You can access an instance in any status.
giaInstanceId :: Lens' GetInstanceAccess Text
giaInstanceId = lens _giaInstanceId (\s a -> s {_giaInstanceId = a})

instance AWSRequest GetInstanceAccess where
  type Rs GetInstanceAccess = GetInstanceAccessResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          GetInstanceAccessResponse'
            <$> (x .?> "InstanceAccess") <*> (pure (fromEnum s))
      )

instance Hashable GetInstanceAccess

instance NFData GetInstanceAccess

instance ToHeaders GetInstanceAccess where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("GameLift.GetInstanceAccess" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetInstanceAccess where
  toJSON GetInstanceAccess' {..} =
    object
      ( catMaybes
          [ Just ("FleetId" .= _giaFleetId),
            Just ("InstanceId" .= _giaInstanceId)
          ]
      )

instance ToPath GetInstanceAccess where
  toPath = const "/"

instance ToQuery GetInstanceAccess where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'getInstanceAccessResponse' smart constructor.
data GetInstanceAccessResponse = GetInstanceAccessResponse'
  { _giarsInstanceAccess ::
      !(Maybe InstanceAccess),
    _giarsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetInstanceAccessResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giarsInstanceAccess' - The connection information for a fleet instance, including IP address and access credentials.
--
-- * 'giarsResponseStatus' - -- | The response status code.
getInstanceAccessResponse ::
  -- | 'giarsResponseStatus'
  Int ->
  GetInstanceAccessResponse
getInstanceAccessResponse pResponseStatus_ =
  GetInstanceAccessResponse'
    { _giarsInstanceAccess = Nothing,
      _giarsResponseStatus = pResponseStatus_
    }

-- | The connection information for a fleet instance, including IP address and access credentials.
giarsInstanceAccess :: Lens' GetInstanceAccessResponse (Maybe InstanceAccess)
giarsInstanceAccess = lens _giarsInstanceAccess (\s a -> s {_giarsInstanceAccess = a})

-- | -- | The response status code.
giarsResponseStatus :: Lens' GetInstanceAccessResponse Int
giarsResponseStatus = lens _giarsResponseStatus (\s a -> s {_giarsResponseStatus = a})

instance NFData GetInstanceAccessResponse
