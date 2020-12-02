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
-- Module      : Network.AWS.EC2.SendDiagnosticInterrupt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a diagnostic interrupt to the specified Amazon EC2 instance to trigger a /kernel panic/ (on Linux instances), or a /blue screen/ //stop error/ (on Windows instances). For instances based on Intel and AMD processors, the interrupt is received as a /non-maskable interrupt/ (NMI).
--
--
-- In general, the operating system crashes and reboots when a kernel panic or stop error is triggered. The operating system can also be configured to perform diagnostic tasks, such as generating a memory dump file, loading a secondary kernel, or obtaining a call trace.
--
-- Before sending a diagnostic interrupt to your instance, ensure that its operating system is configured to perform the required diagnostic tasks.
--
-- For more information about configuring your operating system to generate a crash dump when a kernel panic or stop error occurs, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/diagnostic-interrupt.html Send a diagnostic interrupt> (Linux instances) or <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/diagnostic-interrupt.html Send a Diagnostic Interrupt> (Windows instances).
module Network.AWS.EC2.SendDiagnosticInterrupt
  ( -- * Creating a Request
    sendDiagnosticInterrupt,
    SendDiagnosticInterrupt,

    -- * Request Lenses
    sdiDryRun,
    sdiInstanceId,

    -- * Destructuring the Response
    sendDiagnosticInterruptResponse,
    SendDiagnosticInterruptResponse,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sendDiagnosticInterrupt' smart constructor.
data SendDiagnosticInterrupt = SendDiagnosticInterrupt'
  { _sdiDryRun ::
      !(Maybe Bool),
    _sdiInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendDiagnosticInterrupt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'sdiInstanceId' - The ID of the instance.
sendDiagnosticInterrupt ::
  -- | 'sdiInstanceId'
  Text ->
  SendDiagnosticInterrupt
sendDiagnosticInterrupt pInstanceId_ =
  SendDiagnosticInterrupt'
    { _sdiDryRun = Nothing,
      _sdiInstanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
sdiDryRun :: Lens' SendDiagnosticInterrupt (Maybe Bool)
sdiDryRun = lens _sdiDryRun (\s a -> s {_sdiDryRun = a})

-- | The ID of the instance.
sdiInstanceId :: Lens' SendDiagnosticInterrupt Text
sdiInstanceId = lens _sdiInstanceId (\s a -> s {_sdiInstanceId = a})

instance AWSRequest SendDiagnosticInterrupt where
  type Rs SendDiagnosticInterrupt = SendDiagnosticInterruptResponse
  request = postQuery ec2
  response = receiveNull SendDiagnosticInterruptResponse'

instance Hashable SendDiagnosticInterrupt

instance NFData SendDiagnosticInterrupt

instance ToHeaders SendDiagnosticInterrupt where
  toHeaders = const mempty

instance ToPath SendDiagnosticInterrupt where
  toPath = const "/"

instance ToQuery SendDiagnosticInterrupt where
  toQuery SendDiagnosticInterrupt' {..} =
    mconcat
      [ "Action" =: ("SendDiagnosticInterrupt" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _sdiDryRun,
        "InstanceId" =: _sdiInstanceId
      ]

-- | /See:/ 'sendDiagnosticInterruptResponse' smart constructor.
data SendDiagnosticInterruptResponse = SendDiagnosticInterruptResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendDiagnosticInterruptResponse' with the minimum fields required to make a request.
sendDiagnosticInterruptResponse ::
  SendDiagnosticInterruptResponse
sendDiagnosticInterruptResponse = SendDiagnosticInterruptResponse'

instance NFData SendDiagnosticInterruptResponse
