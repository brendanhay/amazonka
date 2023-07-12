{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2InstanceConnect.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2InstanceConnect.Lens
  ( -- * Operations

    -- ** SendSSHPublicKey
    sendSSHPublicKey_availabilityZone,
    sendSSHPublicKey_instanceId,
    sendSSHPublicKey_instanceOSUser,
    sendSSHPublicKey_sSHPublicKey,
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

import Amazonka.EC2InstanceConnect.SendSSHPublicKey
import Amazonka.EC2InstanceConnect.SendSerialConsoleSSHPublicKey
