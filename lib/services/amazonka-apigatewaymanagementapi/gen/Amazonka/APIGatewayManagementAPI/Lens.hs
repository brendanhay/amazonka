{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGatewayManagementAPI.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGatewayManagementAPI.Lens
  ( -- * Operations

    -- ** DeleteConnection
    deleteConnection_connectionId,

    -- ** GetConnection
    getConnection_connectionId,
    getConnectionResponse_connectedAt,
    getConnectionResponse_identity,
    getConnectionResponse_lastActiveAt,
    getConnectionResponse_httpStatus,

    -- ** PostToConnection
    postToConnection_connectionId,
    postToConnection_data,

    -- * Types

    -- ** Identity
    identity_sourceIp,
    identity_userAgent,
  )
where

import Amazonka.APIGatewayManagementAPI.DeleteConnection
import Amazonka.APIGatewayManagementAPI.GetConnection
import Amazonka.APIGatewayManagementAPI.PostToConnection
import Amazonka.APIGatewayManagementAPI.Types.Identity
