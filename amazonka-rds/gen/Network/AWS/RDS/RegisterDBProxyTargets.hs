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
-- Module      : Network.AWS.RDS.RegisterDBProxyTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate one or more @DBProxyTarget@ data structures with a
-- @DBProxyTargetGroup@.
module Network.AWS.RDS.RegisterDBProxyTargets
  ( -- * Creating a Request
    RegisterDBProxyTargets (..),
    newRegisterDBProxyTargets,

    -- * Request Lenses
    registerDBProxyTargets_dbClusterIdentifiers,
    registerDBProxyTargets_targetGroupName,
    registerDBProxyTargets_dbInstanceIdentifiers,
    registerDBProxyTargets_dbProxyName,

    -- * Destructuring the Response
    RegisterDBProxyTargetsResponse (..),
    newRegisterDBProxyTargetsResponse,

    -- * Response Lenses
    registerDBProxyTargetsResponse_dbProxyTargets,
    registerDBProxyTargetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterDBProxyTargets' smart constructor.
data RegisterDBProxyTargets = RegisterDBProxyTargets'
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
-- Create a value of 'RegisterDBProxyTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterIdentifiers', 'registerDBProxyTargets_dbClusterIdentifiers' - One or more DB cluster identifiers.
--
-- 'targetGroupName', 'registerDBProxyTargets_targetGroupName' - The identifier of the @DBProxyTargetGroup@.
--
-- 'dbInstanceIdentifiers', 'registerDBProxyTargets_dbInstanceIdentifiers' - One or more DB instance identifiers.
--
-- 'dbProxyName', 'registerDBProxyTargets_dbProxyName' - The identifier of the @DBProxy@ that is associated with the
-- @DBProxyTargetGroup@.
newRegisterDBProxyTargets ::
  -- | 'dbProxyName'
  Core.Text ->
  RegisterDBProxyTargets
newRegisterDBProxyTargets pDBProxyName_ =
  RegisterDBProxyTargets'
    { dbClusterIdentifiers =
        Core.Nothing,
      targetGroupName = Core.Nothing,
      dbInstanceIdentifiers = Core.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | One or more DB cluster identifiers.
registerDBProxyTargets_dbClusterIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe [Core.Text])
registerDBProxyTargets_dbClusterIdentifiers = Lens.lens (\RegisterDBProxyTargets' {dbClusterIdentifiers} -> dbClusterIdentifiers) (\s@RegisterDBProxyTargets' {} a -> s {dbClusterIdentifiers = a} :: RegisterDBProxyTargets) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the @DBProxyTargetGroup@.
registerDBProxyTargets_targetGroupName :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe Core.Text)
registerDBProxyTargets_targetGroupName = Lens.lens (\RegisterDBProxyTargets' {targetGroupName} -> targetGroupName) (\s@RegisterDBProxyTargets' {} a -> s {targetGroupName = a} :: RegisterDBProxyTargets)

-- | One or more DB instance identifiers.
registerDBProxyTargets_dbInstanceIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Core.Maybe [Core.Text])
registerDBProxyTargets_dbInstanceIdentifiers = Lens.lens (\RegisterDBProxyTargets' {dbInstanceIdentifiers} -> dbInstanceIdentifiers) (\s@RegisterDBProxyTargets' {} a -> s {dbInstanceIdentifiers = a} :: RegisterDBProxyTargets) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the @DBProxy@ that is associated with the
-- @DBProxyTargetGroup@.
registerDBProxyTargets_dbProxyName :: Lens.Lens' RegisterDBProxyTargets Core.Text
registerDBProxyTargets_dbProxyName = Lens.lens (\RegisterDBProxyTargets' {dbProxyName} -> dbProxyName) (\s@RegisterDBProxyTargets' {} a -> s {dbProxyName = a} :: RegisterDBProxyTargets)

instance Core.AWSRequest RegisterDBProxyTargets where
  type
    AWSResponse RegisterDBProxyTargets =
      RegisterDBProxyTargetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RegisterDBProxyTargetsResult"
      ( \s h x ->
          RegisterDBProxyTargetsResponse'
            Core.<$> ( x Core..@? "DBProxyTargets" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterDBProxyTargets

instance Core.NFData RegisterDBProxyTargets

instance Core.ToHeaders RegisterDBProxyTargets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RegisterDBProxyTargets where
  toPath = Core.const "/"

instance Core.ToQuery RegisterDBProxyTargets where
  toQuery RegisterDBProxyTargets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("RegisterDBProxyTargets" :: Core.ByteString),
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

-- | /See:/ 'newRegisterDBProxyTargetsResponse' smart constructor.
data RegisterDBProxyTargetsResponse = RegisterDBProxyTargetsResponse'
  { -- | One or more @DBProxyTarget@ objects that are created when you register
    -- targets with a target group.
    dbProxyTargets :: Core.Maybe [DBProxyTarget],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterDBProxyTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyTargets', 'registerDBProxyTargetsResponse_dbProxyTargets' - One or more @DBProxyTarget@ objects that are created when you register
-- targets with a target group.
--
-- 'httpStatus', 'registerDBProxyTargetsResponse_httpStatus' - The response's http status code.
newRegisterDBProxyTargetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterDBProxyTargetsResponse
newRegisterDBProxyTargetsResponse pHttpStatus_ =
  RegisterDBProxyTargetsResponse'
    { dbProxyTargets =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | One or more @DBProxyTarget@ objects that are created when you register
-- targets with a target group.
registerDBProxyTargetsResponse_dbProxyTargets :: Lens.Lens' RegisterDBProxyTargetsResponse (Core.Maybe [DBProxyTarget])
registerDBProxyTargetsResponse_dbProxyTargets = Lens.lens (\RegisterDBProxyTargetsResponse' {dbProxyTargets} -> dbProxyTargets) (\s@RegisterDBProxyTargetsResponse' {} a -> s {dbProxyTargets = a} :: RegisterDBProxyTargetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
registerDBProxyTargetsResponse_httpStatus :: Lens.Lens' RegisterDBProxyTargetsResponse Core.Int
registerDBProxyTargetsResponse_httpStatus = Lens.lens (\RegisterDBProxyTargetsResponse' {httpStatus} -> httpStatus) (\s@RegisterDBProxyTargetsResponse' {} a -> s {httpStatus = a} :: RegisterDBProxyTargetsResponse)

instance Core.NFData RegisterDBProxyTargetsResponse
