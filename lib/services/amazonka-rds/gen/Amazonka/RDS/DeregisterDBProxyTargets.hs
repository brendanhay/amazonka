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
-- Module      : Amazonka.RDS.DeregisterDBProxyTargets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the association between one or more @DBProxyTarget@ data
-- structures and a @DBProxyTargetGroup@.
module Amazonka.RDS.DeregisterDBProxyTargets
  ( -- * Creating a Request
    DeregisterDBProxyTargets (..),
    newDeregisterDBProxyTargets,

    -- * Request Lenses
    deregisterDBProxyTargets_dbInstanceIdentifiers,
    deregisterDBProxyTargets_targetGroupName,
    deregisterDBProxyTargets_dbClusterIdentifiers,
    deregisterDBProxyTargets_dbProxyName,

    -- * Destructuring the Response
    DeregisterDBProxyTargetsResponse (..),
    newDeregisterDBProxyTargetsResponse,

    -- * Response Lenses
    deregisterDBProxyTargetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterDBProxyTargets' smart constructor.
data DeregisterDBProxyTargets = DeregisterDBProxyTargets'
  { -- | One or more DB instance identifiers.
    dbInstanceIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the @DBProxyTargetGroup@.
    targetGroupName :: Prelude.Maybe Prelude.Text,
    -- | One or more DB cluster identifiers.
    dbClusterIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the @DBProxy@ that is associated with the
    -- @DBProxyTargetGroup@.
    dbProxyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterDBProxyTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstanceIdentifiers', 'deregisterDBProxyTargets_dbInstanceIdentifiers' - One or more DB instance identifiers.
--
-- 'targetGroupName', 'deregisterDBProxyTargets_targetGroupName' - The identifier of the @DBProxyTargetGroup@.
--
-- 'dbClusterIdentifiers', 'deregisterDBProxyTargets_dbClusterIdentifiers' - One or more DB cluster identifiers.
--
-- 'dbProxyName', 'deregisterDBProxyTargets_dbProxyName' - The identifier of the @DBProxy@ that is associated with the
-- @DBProxyTargetGroup@.
newDeregisterDBProxyTargets ::
  -- | 'dbProxyName'
  Prelude.Text ->
  DeregisterDBProxyTargets
newDeregisterDBProxyTargets pDBProxyName_ =
  DeregisterDBProxyTargets'
    { dbInstanceIdentifiers =
        Prelude.Nothing,
      targetGroupName = Prelude.Nothing,
      dbClusterIdentifiers = Prelude.Nothing,
      dbProxyName = pDBProxyName_
    }

-- | One or more DB instance identifiers.
deregisterDBProxyTargets_dbInstanceIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Prelude.Maybe [Prelude.Text])
deregisterDBProxyTargets_dbInstanceIdentifiers = Lens.lens (\DeregisterDBProxyTargets' {dbInstanceIdentifiers} -> dbInstanceIdentifiers) (\s@DeregisterDBProxyTargets' {} a -> s {dbInstanceIdentifiers = a} :: DeregisterDBProxyTargets) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the @DBProxyTargetGroup@.
deregisterDBProxyTargets_targetGroupName :: Lens.Lens' DeregisterDBProxyTargets (Prelude.Maybe Prelude.Text)
deregisterDBProxyTargets_targetGroupName = Lens.lens (\DeregisterDBProxyTargets' {targetGroupName} -> targetGroupName) (\s@DeregisterDBProxyTargets' {} a -> s {targetGroupName = a} :: DeregisterDBProxyTargets)

-- | One or more DB cluster identifiers.
deregisterDBProxyTargets_dbClusterIdentifiers :: Lens.Lens' DeregisterDBProxyTargets (Prelude.Maybe [Prelude.Text])
deregisterDBProxyTargets_dbClusterIdentifiers = Lens.lens (\DeregisterDBProxyTargets' {dbClusterIdentifiers} -> dbClusterIdentifiers) (\s@DeregisterDBProxyTargets' {} a -> s {dbClusterIdentifiers = a} :: DeregisterDBProxyTargets) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the @DBProxy@ that is associated with the
-- @DBProxyTargetGroup@.
deregisterDBProxyTargets_dbProxyName :: Lens.Lens' DeregisterDBProxyTargets Prelude.Text
deregisterDBProxyTargets_dbProxyName = Lens.lens (\DeregisterDBProxyTargets' {dbProxyName} -> dbProxyName) (\s@DeregisterDBProxyTargets' {} a -> s {dbProxyName = a} :: DeregisterDBProxyTargets)

instance Core.AWSRequest DeregisterDBProxyTargets where
  type
    AWSResponse DeregisterDBProxyTargets =
      DeregisterDBProxyTargetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeregisterDBProxyTargetsResult"
      ( \s h x ->
          DeregisterDBProxyTargetsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterDBProxyTargets where
  hashWithSalt _salt DeregisterDBProxyTargets' {..} =
    _salt `Prelude.hashWithSalt` dbInstanceIdentifiers
      `Prelude.hashWithSalt` targetGroupName
      `Prelude.hashWithSalt` dbClusterIdentifiers
      `Prelude.hashWithSalt` dbProxyName

instance Prelude.NFData DeregisterDBProxyTargets where
  rnf DeregisterDBProxyTargets' {..} =
    Prelude.rnf dbInstanceIdentifiers
      `Prelude.seq` Prelude.rnf targetGroupName
      `Prelude.seq` Prelude.rnf dbClusterIdentifiers
      `Prelude.seq` Prelude.rnf dbProxyName

instance Core.ToHeaders DeregisterDBProxyTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeregisterDBProxyTargets where
  toPath = Prelude.const "/"

instance Core.ToQuery DeregisterDBProxyTargets where
  toQuery DeregisterDBProxyTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeregisterDBProxyTargets" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DBInstanceIdentifiers"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> dbInstanceIdentifiers
            ),
        "TargetGroupName" Core.=: targetGroupName,
        "DBClusterIdentifiers"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> dbClusterIdentifiers
            ),
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newDeregisterDBProxyTargetsResponse' smart constructor.
data DeregisterDBProxyTargetsResponse = DeregisterDBProxyTargetsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeregisterDBProxyTargetsResponse
newDeregisterDBProxyTargetsResponse pHttpStatus_ =
  DeregisterDBProxyTargetsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterDBProxyTargetsResponse_httpStatus :: Lens.Lens' DeregisterDBProxyTargetsResponse Prelude.Int
deregisterDBProxyTargetsResponse_httpStatus = Lens.lens (\DeregisterDBProxyTargetsResponse' {httpStatus} -> httpStatus) (\s@DeregisterDBProxyTargetsResponse' {} a -> s {httpStatus = a} :: DeregisterDBProxyTargetsResponse)

instance
  Prelude.NFData
    DeregisterDBProxyTargetsResponse
  where
  rnf DeregisterDBProxyTargetsResponse' {..} =
    Prelude.rnf httpStatus
