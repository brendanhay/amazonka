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
-- Module      : Amazonka.RDS.RegisterDBProxyTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate one or more @DBProxyTarget@ data structures with a
-- @DBProxyTargetGroup@.
module Amazonka.RDS.RegisterDBProxyTargets
  ( -- * Creating a Request
    RegisterDBProxyTargets (..),
    newRegisterDBProxyTargets,

    -- * Request Lenses
    registerDBProxyTargets_dbClusterIdentifiers,
    registerDBProxyTargets_dbInstanceIdentifiers,
    registerDBProxyTargets_targetGroupName,
    registerDBProxyTargets_dbProxyName,

    -- * Destructuring the Response
    RegisterDBProxyTargetsResponse (..),
    newRegisterDBProxyTargetsResponse,

    -- * Response Lenses
    registerDBProxyTargetsResponse_dbProxyTargets,
    registerDBProxyTargetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterDBProxyTargets' smart constructor.
data RegisterDBProxyTargets = RegisterDBProxyTargets'
  { -- | One or more DB cluster identifiers.
    dbClusterIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | One or more DB instance identifiers.
    dbInstanceIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the @DBProxyTargetGroup@.
    targetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the @DBProxy@ that is associated with the
    -- @DBProxyTargetGroup@.
    dbProxyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'dbInstanceIdentifiers', 'registerDBProxyTargets_dbInstanceIdentifiers' - One or more DB instance identifiers.
--
-- 'targetGroupName', 'registerDBProxyTargets_targetGroupName' - The identifier of the @DBProxyTargetGroup@.
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
      dbInstanceIdentifiers = Prelude.Nothing,
      targetGroupName = Prelude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | One or more DB cluster identifiers.
registerDBProxyTargets_dbClusterIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Prelude.Maybe [Prelude.Text])
registerDBProxyTargets_dbClusterIdentifiers = Lens.lens (\RegisterDBProxyTargets' {dbClusterIdentifiers} -> dbClusterIdentifiers) (\s@RegisterDBProxyTargets' {} a -> s {dbClusterIdentifiers = a} :: RegisterDBProxyTargets) Prelude.. Lens.mapping Lens.coerced

-- | One or more DB instance identifiers.
registerDBProxyTargets_dbInstanceIdentifiers :: Lens.Lens' RegisterDBProxyTargets (Prelude.Maybe [Prelude.Text])
registerDBProxyTargets_dbInstanceIdentifiers = Lens.lens (\RegisterDBProxyTargets' {dbInstanceIdentifiers} -> dbInstanceIdentifiers) (\s@RegisterDBProxyTargets' {} a -> s {dbInstanceIdentifiers = a} :: RegisterDBProxyTargets) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the @DBProxyTargetGroup@.
registerDBProxyTargets_targetGroupName :: Lens.Lens' RegisterDBProxyTargets (Prelude.Maybe Prelude.Text)
registerDBProxyTargets_targetGroupName = Lens.lens (\RegisterDBProxyTargets' {targetGroupName} -> targetGroupName) (\s@RegisterDBProxyTargets' {} a -> s {targetGroupName = a} :: RegisterDBProxyTargets)

-- | The identifier of the @DBProxy@ that is associated with the
-- @DBProxyTargetGroup@.
registerDBProxyTargets_dbProxyName :: Lens.Lens' RegisterDBProxyTargets Prelude.Text
registerDBProxyTargets_dbProxyName = Lens.lens (\RegisterDBProxyTargets' {dbProxyName} -> dbProxyName) (\s@RegisterDBProxyTargets' {} a -> s {dbProxyName = a} :: RegisterDBProxyTargets)

instance Core.AWSRequest RegisterDBProxyTargets where
  type
    AWSResponse RegisterDBProxyTargets =
      RegisterDBProxyTargetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RegisterDBProxyTargetsResult"
      ( \s h x ->
          RegisterDBProxyTargetsResponse'
            Prelude.<$> ( x Data..@? "DBProxyTargets" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterDBProxyTargets where
  hashWithSalt _salt RegisterDBProxyTargets' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterIdentifiers
      `Prelude.hashWithSalt` dbInstanceIdentifiers
      `Prelude.hashWithSalt` targetGroupName
      `Prelude.hashWithSalt` dbProxyName

instance Prelude.NFData RegisterDBProxyTargets where
  rnf RegisterDBProxyTargets' {..} =
    Prelude.rnf dbClusterIdentifiers `Prelude.seq`
      Prelude.rnf dbInstanceIdentifiers `Prelude.seq`
        Prelude.rnf targetGroupName `Prelude.seq`
          Prelude.rnf dbProxyName

instance Data.ToHeaders RegisterDBProxyTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RegisterDBProxyTargets where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterDBProxyTargets where
  toQuery RegisterDBProxyTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RegisterDBProxyTargets" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterIdentifiers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> dbClusterIdentifiers
            ),
        "DBInstanceIdentifiers"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> dbInstanceIdentifiers
            ),
        "TargetGroupName" Data.=: targetGroupName,
        "DBProxyName" Data.=: dbProxyName
      ]

-- | /See:/ 'newRegisterDBProxyTargetsResponse' smart constructor.
data RegisterDBProxyTargetsResponse = RegisterDBProxyTargetsResponse'
  { -- | One or more @DBProxyTarget@ objects that are created when you register
    -- targets with a target group.
    dbProxyTargets :: Prelude.Maybe [DBProxyTarget],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
registerDBProxyTargetsResponse_dbProxyTargets = Lens.lens (\RegisterDBProxyTargetsResponse' {dbProxyTargets} -> dbProxyTargets) (\s@RegisterDBProxyTargetsResponse' {} a -> s {dbProxyTargets = a} :: RegisterDBProxyTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
registerDBProxyTargetsResponse_httpStatus :: Lens.Lens' RegisterDBProxyTargetsResponse Prelude.Int
registerDBProxyTargetsResponse_httpStatus = Lens.lens (\RegisterDBProxyTargetsResponse' {httpStatus} -> httpStatus) (\s@RegisterDBProxyTargetsResponse' {} a -> s {httpStatus = a} :: RegisterDBProxyTargetsResponse)

instance
  Prelude.NFData
    RegisterDBProxyTargetsResponse
  where
  rnf RegisterDBProxyTargetsResponse' {..} =
    Prelude.rnf dbProxyTargets `Prelude.seq`
      Prelude.rnf httpStatus
