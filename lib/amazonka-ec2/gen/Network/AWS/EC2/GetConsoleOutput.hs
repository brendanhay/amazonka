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
-- Module      : Network.AWS.EC2.GetConsoleOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the console output for the specified instance. For Linux instances, the instance console output displays the exact console output that would normally be displayed on a physical monitor attached to a computer. For Windows instances, the instance console output includes the last three system event log errors.
--
--
-- By default, the console output returns buffered information that was posted shortly after an instance transition state (start, stop, reboot, or terminate). This information is available for at least one hour after the most recent post. Only the most recent 64 KB of console output is available.
--
-- You can optionally retrieve the latest serial console output at any time during the instance lifecycle. This option is supported on instance types that use the Nitro hypervisor.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-console.html#instance-console-console-output Instance Console Output> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetConsoleOutput
  ( -- * Creating a Request
    getConsoleOutput,
    GetConsoleOutput,

    -- * Request Lenses
    gcoLatest,
    gcoDryRun,
    gcoInstanceId,

    -- * Destructuring the Response
    getConsoleOutputResponse,
    GetConsoleOutputResponse,

    -- * Response Lenses
    gcorsInstanceId,
    gcorsOutput,
    gcorsTimestamp,
    gcorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConsoleOutput' smart constructor.
data GetConsoleOutput = GetConsoleOutput'
  { _gcoLatest ::
      !(Maybe Bool),
    _gcoDryRun :: !(Maybe Bool),
    _gcoInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConsoleOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcoLatest' - When enabled, retrieves the latest console output for the instance. Default: disabled (@false@ )
--
-- * 'gcoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gcoInstanceId' - The ID of the instance.
getConsoleOutput ::
  -- | 'gcoInstanceId'
  Text ->
  GetConsoleOutput
getConsoleOutput pInstanceId_ =
  GetConsoleOutput'
    { _gcoLatest = Nothing,
      _gcoDryRun = Nothing,
      _gcoInstanceId = pInstanceId_
    }

-- | When enabled, retrieves the latest console output for the instance. Default: disabled (@false@ )
gcoLatest :: Lens' GetConsoleOutput (Maybe Bool)
gcoLatest = lens _gcoLatest (\s a -> s {_gcoLatest = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gcoDryRun :: Lens' GetConsoleOutput (Maybe Bool)
gcoDryRun = lens _gcoDryRun (\s a -> s {_gcoDryRun = a})

-- | The ID of the instance.
gcoInstanceId :: Lens' GetConsoleOutput Text
gcoInstanceId = lens _gcoInstanceId (\s a -> s {_gcoInstanceId = a})

instance AWSRequest GetConsoleOutput where
  type Rs GetConsoleOutput = GetConsoleOutputResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetConsoleOutputResponse'
            <$> (x .@? "instanceId")
            <*> (x .@? "output")
            <*> (x .@? "timestamp")
            <*> (pure (fromEnum s))
      )

instance Hashable GetConsoleOutput

instance NFData GetConsoleOutput

instance ToHeaders GetConsoleOutput where
  toHeaders = const mempty

instance ToPath GetConsoleOutput where
  toPath = const "/"

instance ToQuery GetConsoleOutput where
  toQuery GetConsoleOutput' {..} =
    mconcat
      [ "Action" =: ("GetConsoleOutput" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "Latest" =: _gcoLatest,
        "DryRun" =: _gcoDryRun,
        "InstanceId" =: _gcoInstanceId
      ]

-- | /See:/ 'getConsoleOutputResponse' smart constructor.
data GetConsoleOutputResponse = GetConsoleOutputResponse'
  { _gcorsInstanceId ::
      !(Maybe Text),
    _gcorsOutput :: !(Maybe Text),
    _gcorsTimestamp :: !(Maybe ISO8601),
    _gcorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConsoleOutputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcorsInstanceId' - The ID of the instance.
--
-- * 'gcorsOutput' - The console output, base64-encoded. If you are using a command line tool, the tool decodes the output for you.
--
-- * 'gcorsTimestamp' - The time at which the output was last updated.
--
-- * 'gcorsResponseStatus' - -- | The response status code.
getConsoleOutputResponse ::
  -- | 'gcorsResponseStatus'
  Int ->
  GetConsoleOutputResponse
getConsoleOutputResponse pResponseStatus_ =
  GetConsoleOutputResponse'
    { _gcorsInstanceId = Nothing,
      _gcorsOutput = Nothing,
      _gcorsTimestamp = Nothing,
      _gcorsResponseStatus = pResponseStatus_
    }

-- | The ID of the instance.
gcorsInstanceId :: Lens' GetConsoleOutputResponse (Maybe Text)
gcorsInstanceId = lens _gcorsInstanceId (\s a -> s {_gcorsInstanceId = a})

-- | The console output, base64-encoded. If you are using a command line tool, the tool decodes the output for you.
gcorsOutput :: Lens' GetConsoleOutputResponse (Maybe Text)
gcorsOutput = lens _gcorsOutput (\s a -> s {_gcorsOutput = a})

-- | The time at which the output was last updated.
gcorsTimestamp :: Lens' GetConsoleOutputResponse (Maybe UTCTime)
gcorsTimestamp = lens _gcorsTimestamp (\s a -> s {_gcorsTimestamp = a}) . mapping _Time

-- | -- | The response status code.
gcorsResponseStatus :: Lens' GetConsoleOutputResponse Int
gcorsResponseStatus = lens _gcorsResponseStatus (\s a -> s {_gcorsResponseStatus = a})

instance NFData GetConsoleOutputResponse
