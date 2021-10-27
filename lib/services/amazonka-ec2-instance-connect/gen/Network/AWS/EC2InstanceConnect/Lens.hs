{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2InstanceConnect.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2InstanceConnect.Lens
  ( -- * Operations

    -- ** SendSSHPublicKey
    sendSSHPublicKey_instanceId,
    sendSSHPublicKey_instanceOSUser,
    sendSSHPublicKey_sSHPublicKey,
    sendSSHPublicKey_availabilityZone,
    sendSSHPublicKeyResponse_requestId,
    sendSSHPublicKeyResponse_success,
    sendSSHPublicKeyResponse_httpStatus,

    -- ** SendSerialConsoleSSHPublicKey
    sendSerialConsoleSSHPublicKey_serialPort,
    sendSerialConsoleSSHPublicKey_instanceId,
    sendSerialConsoleSSHPublicKey_sSHPublicKey,
    sendSerialConsoleSSHPublicKeyResponse_requestId,
    sendSerialConsoleSSHPublicKeyResponse_success,
    sendSerialConsoleSSHPublicKeyResponse_httpStatus,

    -- * Types
  )
where

import Network.AWS.EC2InstanceConnect.SendSSHPublicKey
import Network.AWS.EC2InstanceConnect.SendSerialConsoleSSHPublicKey
