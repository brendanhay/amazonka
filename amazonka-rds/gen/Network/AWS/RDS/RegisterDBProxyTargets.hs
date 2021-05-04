{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterDBProxyTargets' smart constructor.
data RegisterDBProxyTargets = RegisterDBProxyTargets'
  { -- | One or more DB cluster identifiers.
    dbClusterIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the @DBProxyTargetGroup@.
    targetGroupName :: Prelude.Maybe Prelude.Text,
    -- | One or more DB instance identifiers.
    dbInstanceIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the @DBProxy@ that is associated with the
    -- @DBProxyTargetGroup@.
    dbProxyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  RegisterDBProxyTargets
newRegisterDBProxyTargets pDBProxyName_ =
  RegisterDBProxyTargets'
    { dbClusterIdentifiers =
        Prelude.Nothing,
      targetGroupName = Prelude.Nothing,
      dbInstanceIdentifiers = Prelude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | One or more DB cluster identifiers.
registerDBProxyTargets_dbClusterIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Prelude.Maybe [Prelude.Text])
registerDBProxyTargets_dbClusterIdentifiers = Lens.lens (\RegisterDBProxyTargets' {dbClusterIdentifiers} -> dbClusterIdentifiers) (\s@RegisterDBProxyTargets' {} a -> s {dbClusterIdentifiers = a} :: RegisterDBProxyTargets) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifier of the @DBProxyTargetGroup@.
registerDBProxyTargets_targetGroupName :: Lens.Lens' RegisterDBProxyTargets (Prelude.Maybe Prelude.Text)
registerDBProxyTargets_targetGroupName = Lens.lens (\RegisterDBProxyTargets' {targetGroupName} -> targetGroupName) (\s@RegisterDBProxyTargets' {} a -> s {targetGroupName = a} :: RegisterDBProxyTargets)

-- | One or more DB instance identifiers.
registerDBProxyTargets_dbInstanceIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Prelude.Maybe [Prelude.Text])
registerDBProxyTargets_dbInstanceIdentifiers = Lens.lens (\RegisterDBProxyTargets' {dbInstanceIdentifiers} -> dbInstanceIdentifiers) (\s@RegisterDBProxyTargets' {} a -> s {dbInstanceIdentifiers = a} :: RegisterDBProxyTargets) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifier of the @DBProxy@ that is associated with the
-- @DBProxyTargetGroup@.
registerDBProxyTargets_dbProxyName :: Lens.Lens' RegisterDBProxyTargets Prelude.Text
registerDBProxyTargets_dbProxyName = Lens.lens (\RegisterDBProxyTargets' {dbProxyName} -> dbProxyName) (\s@RegisterDBProxyTargets' {} a -> s {dbProxyName = a} :: RegisterDBProxyTargets)

instance Prelude.AWSRequest RegisterDBProxyTargets where
  type
    Rs RegisterDBProxyTargets =
      RegisterDBProxyTargetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RegisterDBProxyTargetsResult"
      ( \s h x ->
          RegisterDBProxyTargetsResponse'
            Prelude.<$> ( x Prelude..@? "DBProxyTargets"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterDBProxyTargets

instance Prelude.NFData RegisterDBProxyTargets

instance Prelude.ToHeaders RegisterDBProxyTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RegisterDBProxyTargets where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterDBProxyTargets where
  toQuery RegisterDBProxyTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RegisterDBProxyTargets" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterIdentifiers"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> dbClusterIdentifiers
            ),
        "TargetGroupName" Prelude.=: targetGroupName,
        "DBInstanceIdentifiers"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> dbInstanceIdentifiers
            ),
        "DBProxyName" Prelude.=: dbProxyName
      ]

-- | /See:/ 'newRegisterDBProxyTargetsResponse' smart constructor.
data RegisterDBProxyTargetsResponse = RegisterDBProxyTargetsResponse'
  { -- | One or more @DBProxyTarget@ objects that are created when you register
    -- targets with a target group.
    dbProxyTargets :: Prelude.Maybe [DBProxyTarget],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  RegisterDBProxyTargetsResponse
newRegisterDBProxyTargetsResponse pHttpStatus_ =
  RegisterDBProxyTargetsResponse'
    { dbProxyTargets =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | One or more @DBProxyTarget@ objects that are created when you register
-- targets with a target group.
registerDBProxyTargetsResponse_dbProxyTargets :: Lens.Lens' RegisterDBProxyTargetsResponse (Prelude.Maybe [DBProxyTarget])
registerDBProxyTargetsResponse_dbProxyTargets = Lens.lens (\RegisterDBProxyTargetsResponse' {dbProxyTargets} -> dbProxyTargets) (\s@RegisterDBProxyTargetsResponse' {} a -> s {dbProxyTargets = a} :: RegisterDBProxyTargetsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
registerDBProxyTargetsResponse_httpStatus :: Lens.Lens' RegisterDBProxyTargetsResponse Prelude.Int
registerDBProxyTargetsResponse_httpStatus = Lens.lens (\RegisterDBProxyTargetsResponse' {httpStatus} -> httpStatus) (\s@RegisterDBProxyTargetsResponse' {} a -> s {httpStatus = a} :: RegisterDBProxyTargetsResponse)

instance
  Prelude.NFData
    RegisterDBProxyTargetsResponse
