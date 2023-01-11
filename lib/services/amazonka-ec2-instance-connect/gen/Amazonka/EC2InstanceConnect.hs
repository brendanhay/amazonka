{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.EC2InstanceConnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-04-02@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon EC2 Instance Connect enables system administrators to publish
-- one-time use SSH public keys to EC2, providing users a simple and secure
-- way to connect to their instances.
module Amazonka.EC2InstanceConnect
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AuthException
    _AuthException,

    -- ** EC2InstanceNotFoundException
    _EC2InstanceNotFoundException,

    -- ** EC2InstanceStateInvalidException
    _EC2InstanceStateInvalidException,

    -- ** EC2InstanceTypeInvalidException
    _EC2InstanceTypeInvalidException,

    -- ** EC2InstanceUnavailableException
    _EC2InstanceUnavailableException,

    -- ** InvalidArgsException
    _InvalidArgsException,

    -- ** SerialConsoleAccessDisabledException
    _SerialConsoleAccessDisabledException,

    -- ** SerialConsoleSessionLimitExceededException
    _SerialConsoleSessionLimitExceededException,

    -- ** SerialConsoleSessionUnavailableException
    _SerialConsoleSessionUnavailableException,

    -- ** ServiceException
    _ServiceException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** SendSSHPublicKey
    SendSSHPublicKey (SendSSHPublicKey'),
    newSendSSHPublicKey,
    SendSSHPublicKeyResponse (SendSSHPublicKeyResponse'),
    newSendSSHPublicKeyResponse,

    -- ** SendSerialConsoleSSHPublicKey
    SendSerialConsoleSSHPublicKey (SendSerialConsoleSSHPublicKey'),
    newSendSerialConsoleSSHPublicKey,
    SendSerialConsoleSSHPublicKeyResponse (SendSerialConsoleSSHPublicKeyResponse'),
    newSendSerialConsoleSSHPublicKeyResponse,

    -- * Types
  )
where

import Amazonka.EC2InstanceConnect.Lens
import Amazonka.EC2InstanceConnect.SendSSHPublicKey
import Amazonka.EC2InstanceConnect.SendSerialConsoleSSHPublicKey
import Amazonka.EC2InstanceConnect.Types
import Amazonka.EC2InstanceConnect.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'EC2InstanceConnect'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
