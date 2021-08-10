{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGatewayManagementAPI.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGatewayManagementAPI.Lens
  ( -- * Operations

    -- ** DeleteConnection
    deleteConnection_connectionId,

    -- ** PostToConnection
    postToConnection_connectionId,
    postToConnection_data,

    -- ** GetConnection
    getConnection_connectionId,
    getConnectionResponse_identity,
    getConnectionResponse_lastActiveAt,
    getConnectionResponse_connectedAt,
    getConnectionResponse_httpStatus,

    -- * Types

    -- ** Identity
    identity_sourceIp,
    identity_userAgent,
  )
where

import Network.AWS.APIGatewayManagementAPI.DeleteConnection
import Network.AWS.APIGatewayManagementAPI.GetConnection
import Network.AWS.APIGatewayManagementAPI.PostToConnection
import Network.AWS.APIGatewayManagementAPI.Types.Identity
