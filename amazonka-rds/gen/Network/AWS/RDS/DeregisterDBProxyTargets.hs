{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeregisterDBProxyTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the association between one or more @DBProxyTarget@ data
-- structures and a @DBProxyTargetGroup@.
module Network.AWS.RDS.DeregisterDBProxyTargets
  ( -- * Creating a Request
    DeregisterDBProxyTargets (..),
    newDeregisterDBProxyTargets,

    -- * Request Lenses
    deregisterDBProxyTargets_dbClusterIdentifiers,
    deregisterDBProxyTargets_targetGroupName,
    deregisterDBProxyTargets_dbInstanceIdentifiers,
    deregisterDBProxyTargets_dbProxyName,

    -- * Destructuring the Response
    DeregisterDBProxyTargetsResponse (..),
    newDeregisterDBProxyTargetsResponse,

    -- * Response Lenses
    deregisterDBProxyTargetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterDBProxyTargets' smart constructor.
data DeregisterDBProxyTargets = DeregisterDBProxyTargets'
  { -- | One or more DB cluster identifiers.
    dbClusterIdentifiers :: Core.Maybe [Core.Text],
    -- | The identifier of the @DBProxyTargetGroup@.
    targetGroupName :: Core.Maybe Core.Text,
    -- | One or more DB instance identifiers.
    dbInstanceIdentifiers :: Core.Maybe [Core.Text],
    -- | The identifier of the @DBProxy@ that is associated with the
    -- @DBProxyTargetGroup@.
    dbProxyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterDBProxyTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifiers', 'deregisterDBProxyTargets_dbClusterIdentifiers' - One or more DB cluster identifiers.
--
-- 'targetGroupName', 'deregisterDBProxyTargets_targetGroupName' - The identifier of the @DBProxyTargetGroup@.
--
-- 'dbInstanceIdentifiers', 'deregisterDBProxyTargets_dbInstanceIdentifiers' - One or more DB instance identifiers.
--
-- 'dbProxyName', 'deregisterDBProxyTargets_dbProxyName' - The identifier of the @DBProxy@ that is associated with the
-- @DBProxyTargetGroup@.
newDeregisterDBProxyTargets ::
  -- | 'dbProxyName'
  Core.Text ->
  DeregisterDBProxyTargets
newDeregisterDBProxyTargets pDBProxyName_ =
  DeregisterDBProxyTargets'
    { dbClusterIdentifiers =
        Core.Nothing,
      targetGroupName = Core.Nothing,
      dbInstanceIdentifiers = Core.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | One or more DB cluster identifiers.
deregisterDBProxyTargets_dbClusterIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe [Core.Text])
deregisterDBProxyTargets_dbClusterIdentifiers = Lens.lens (\DeregisterDBProxyTargets' {dbClusterIdentifiers} -> dbClusterIdentifiers) (\s@DeregisterDBProxyTargets' {} a -> s {dbClusterIdentifiers = a} :: DeregisterDBProxyTargets) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the @DBProxyTargetGroup@.
deregisterDBProxyTargets_targetGroupName :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe Core.Text)
deregisterDBProxyTargets_targetGroupName = Lens.lens (\DeregisterDBProxyTargets' {targetGroupName} -> targetGroupName) (\s@DeregisterDBProxyTargets' {} a -> s {targetGroupName = a} :: DeregisterDBProxyTargets)

-- | One or more DB instance identifiers.
deregisterDBProxyTargets_dbInstanceIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Core.Maybe [Core.Text])
deregisterDBProxyTargets_dbInstanceIdentifiers = Lens.lens (\DeregisterDBProxyTargets' {dbInstanceIdentifiers} -> dbInstanceIdentifiers) (\s@DeregisterDBProxyTargets' {} a -> s {dbInstanceIdentifiers = a} :: DeregisterDBProxyTargets) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the @DBProxy@ that is associated with the
-- @DBProxyTargetGroup@.
deregisterDBProxyTargets_dbProxyName :: Lens.Lens' DeregisterDBProxyTargets Core.Text
deregisterDBProxyTargets_dbProxyName = Lens.lens (\DeregisterDBProxyTargets' {dbProxyName} -> dbProxyName) (\s@DeregisterDBProxyTargets' {} a -> s {dbProxyName = a} :: DeregisterDBProxyTargets)

instance Core.AWSRequest DeregisterDBProxyTargets where
  type
    AWSResponse DeregisterDBProxyTargets =
      DeregisterDBProxyTargetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeregisterDBProxyTargetsResult"
      ( \s h x ->
          DeregisterDBProxyTargetsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterDBProxyTargets

instance Core.NFData DeregisterDBProxyTargets

instance Core.ToHeaders DeregisterDBProxyTargets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeregisterDBProxyTargets where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterDBProxyTargets where
  toQuery DeregisterDBProxyTargets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeregisterDBProxyTargets" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBClusterIdentifiers"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> dbClusterIdentifiers
            ),
        "TargetGroupName" Core.=: targetGroupName,
        "DBInstanceIdentifiers"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> dbInstanceIdentifiers
            ),
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newDeregisterDBProxyTargetsResponse' smart constructor.
data DeregisterDBProxyTargetsResponse = DeregisterDBProxyTargetsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterDBProxyTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterDBProxyTargetsResponse_httpStatus' - The response's http status code.
newDeregisterDBProxyTargetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeregisterDBProxyTargetsResponse
newDeregisterDBProxyTargetsResponse pHttpStatus_ =
  DeregisterDBProxyTargetsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterDBProxyTargetsResponse_httpStatus :: Lens.Lens' DeregisterDBProxyTargetsResponse Core.Int
deregisterDBProxyTargetsResponse_httpStatus = Lens.lens (\DeregisterDBProxyTargetsResponse' {httpStatus} -> httpStatus) (\s@DeregisterDBProxyTargetsResponse' {} a -> s {httpStatus = a} :: DeregisterDBProxyTargetsResponse)

instance Core.NFData DeregisterDBProxyTargetsResponse
