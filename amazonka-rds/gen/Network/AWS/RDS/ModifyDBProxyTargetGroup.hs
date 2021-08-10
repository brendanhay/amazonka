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
-- Module      : Network.AWS.RDS.ModifyDBProxyTargetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the properties of a @DBProxyTargetGroup@.
module Network.AWS.RDS.ModifyDBProxyTargetGroup
  ( -- * Creating a Request
    ModifyDBProxyTargetGroup (..),
    newModifyDBProxyTargetGroup,

    -- * Request Lenses
    modifyDBProxyTargetGroup_connectionPoolConfig,
    modifyDBProxyTargetGroup_newName,
    modifyDBProxyTargetGroup_targetGroupName,
    modifyDBProxyTargetGroup_dbProxyName,

    -- * Destructuring the Response
    ModifyDBProxyTargetGroupResponse (..),
    newModifyDBProxyTargetGroupResponse,

    -- * Response Lenses
    modifyDBProxyTargetGroupResponse_dbProxyTargetGroup,
    modifyDBProxyTargetGroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyDBProxyTargetGroup' smart constructor.
data ModifyDBProxyTargetGroup = ModifyDBProxyTargetGroup'
  { -- | The settings that determine the size and behavior of the connection pool
    -- for the target group.
    connectionPoolConfig :: Prelude.Maybe ConnectionPoolConfiguration,
    -- | The new name for the modified @DBProxyTarget@. An identifier must begin
    -- with a letter and must contain only ASCII letters, digits, and hyphens;
    -- it can\'t end with a hyphen or contain two consecutive hyphens.
    newName' :: Prelude.Maybe Prelude.Text,
    -- | The name of the new target group to assign to the proxy.
    targetGroupName :: Prelude.Text,
    -- | The name of the new proxy to which to assign the target group.
    dbProxyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBProxyTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionPoolConfig', 'modifyDBProxyTargetGroup_connectionPoolConfig' - The settings that determine the size and behavior of the connection pool
-- for the target group.
--
-- 'newName'', 'modifyDBProxyTargetGroup_newName' - The new name for the modified @DBProxyTarget@. An identifier must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens;
-- it can\'t end with a hyphen or contain two consecutive hyphens.
--
-- 'targetGroupName', 'modifyDBProxyTargetGroup_targetGroupName' - The name of the new target group to assign to the proxy.
--
-- 'dbProxyName', 'modifyDBProxyTargetGroup_dbProxyName' - The name of the new proxy to which to assign the target group.
newModifyDBProxyTargetGroup ::
  -- | 'targetGroupName'
  Prelude.Text ->
  -- | 'dbProxyName'
  Prelude.Text ->
  ModifyDBProxyTargetGroup
newModifyDBProxyTargetGroup
  pTargetGroupName_
  pDBProxyName_ =
    ModifyDBProxyTargetGroup'
      { connectionPoolConfig =
          Prelude.Nothing,
        newName' = Prelude.Nothing,
        targetGroupName = pTargetGroupName_,
        dbProxyName = pDBProxyName_
      }

-- | The settings that determine the size and behavior of the connection pool
-- for the target group.
modifyDBProxyTargetGroup_connectionPoolConfig :: Lens.Lens' ModifyDBProxyTargetGroup (Prelude.Maybe ConnectionPoolConfiguration)
modifyDBProxyTargetGroup_connectionPoolConfig = Lens.lens (\ModifyDBProxyTargetGroup' {connectionPoolConfig} -> connectionPoolConfig) (\s@ModifyDBProxyTargetGroup' {} a -> s {connectionPoolConfig = a} :: ModifyDBProxyTargetGroup)

-- | The new name for the modified @DBProxyTarget@. An identifier must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens;
-- it can\'t end with a hyphen or contain two consecutive hyphens.
modifyDBProxyTargetGroup_newName :: Lens.Lens' ModifyDBProxyTargetGroup (Prelude.Maybe Prelude.Text)
modifyDBProxyTargetGroup_newName = Lens.lens (\ModifyDBProxyTargetGroup' {newName'} -> newName') (\s@ModifyDBProxyTargetGroup' {} a -> s {newName' = a} :: ModifyDBProxyTargetGroup)

-- | The name of the new target group to assign to the proxy.
modifyDBProxyTargetGroup_targetGroupName :: Lens.Lens' ModifyDBProxyTargetGroup Prelude.Text
modifyDBProxyTargetGroup_targetGroupName = Lens.lens (\ModifyDBProxyTargetGroup' {targetGroupName} -> targetGroupName) (\s@ModifyDBProxyTargetGroup' {} a -> s {targetGroupName = a} :: ModifyDBProxyTargetGroup)

-- | The name of the new proxy to which to assign the target group.
modifyDBProxyTargetGroup_dbProxyName :: Lens.Lens' ModifyDBProxyTargetGroup Prelude.Text
modifyDBProxyTargetGroup_dbProxyName = Lens.lens (\ModifyDBProxyTargetGroup' {dbProxyName} -> dbProxyName) (\s@ModifyDBProxyTargetGroup' {} a -> s {dbProxyName = a} :: ModifyDBProxyTargetGroup)

instance Core.AWSRequest ModifyDBProxyTargetGroup where
  type
    AWSResponse ModifyDBProxyTargetGroup =
      ModifyDBProxyTargetGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBProxyTargetGroupResult"
      ( \s h x ->
          ModifyDBProxyTargetGroupResponse'
            Prelude.<$> (x Core..@? "DBProxyTargetGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBProxyTargetGroup

instance Prelude.NFData ModifyDBProxyTargetGroup

instance Core.ToHeaders ModifyDBProxyTargetGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBProxyTargetGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyDBProxyTargetGroup where
  toQuery ModifyDBProxyTargetGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyDBProxyTargetGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "ConnectionPoolConfig" Core.=: connectionPoolConfig,
        "NewName" Core.=: newName',
        "TargetGroupName" Core.=: targetGroupName,
        "DBProxyName" Core.=: dbProxyName
      ]

-- | /See:/ 'newModifyDBProxyTargetGroupResponse' smart constructor.
data ModifyDBProxyTargetGroupResponse = ModifyDBProxyTargetGroupResponse'
  { -- | The settings of the modified @DBProxyTarget@.
    dbProxyTargetGroup :: Prelude.Maybe DBProxyTargetGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBProxyTargetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyTargetGroup', 'modifyDBProxyTargetGroupResponse_dbProxyTargetGroup' - The settings of the modified @DBProxyTarget@.
--
-- 'httpStatus', 'modifyDBProxyTargetGroupResponse_httpStatus' - The response's http status code.
newModifyDBProxyTargetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBProxyTargetGroupResponse
newModifyDBProxyTargetGroupResponse pHttpStatus_ =
  ModifyDBProxyTargetGroupResponse'
    { dbProxyTargetGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The settings of the modified @DBProxyTarget@.
modifyDBProxyTargetGroupResponse_dbProxyTargetGroup :: Lens.Lens' ModifyDBProxyTargetGroupResponse (Prelude.Maybe DBProxyTargetGroup)
modifyDBProxyTargetGroupResponse_dbProxyTargetGroup = Lens.lens (\ModifyDBProxyTargetGroupResponse' {dbProxyTargetGroup} -> dbProxyTargetGroup) (\s@ModifyDBProxyTargetGroupResponse' {} a -> s {dbProxyTargetGroup = a} :: ModifyDBProxyTargetGroupResponse)

-- | The response's http status code.
modifyDBProxyTargetGroupResponse_httpStatus :: Lens.Lens' ModifyDBProxyTargetGroupResponse Prelude.Int
modifyDBProxyTargetGroupResponse_httpStatus = Lens.lens (\ModifyDBProxyTargetGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyDBProxyTargetGroupResponse' {} a -> s {httpStatus = a} :: ModifyDBProxyTargetGroupResponse)

instance
  Prelude.NFData
    ModifyDBProxyTargetGroupResponse
