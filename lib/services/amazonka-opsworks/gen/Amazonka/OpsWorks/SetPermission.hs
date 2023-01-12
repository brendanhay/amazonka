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
-- Module      : Amazonka.OpsWorks.SetPermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies a user\'s permissions. For more information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workingsecurity.html Security and Permissions>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.SetPermission
  ( -- * Creating a Request
    SetPermission (..),
    newSetPermission,

    -- * Request Lenses
    setPermission_allowSsh,
    setPermission_allowSudo,
    setPermission_level,
    setPermission_stackId,
    setPermission_iamUserArn,

    -- * Destructuring the Response
    SetPermissionResponse (..),
    newSetPermissionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetPermission' smart constructor.
data SetPermission = SetPermission'
  { -- | The user is allowed to use SSH to communicate with the instance.
    allowSsh :: Prelude.Maybe Prelude.Bool,
    -- | The user is allowed to use __sudo__ to elevate privileges.
    allowSudo :: Prelude.Maybe Prelude.Bool,
    -- | The user\'s permission level, which must be set to one of the following
    -- strings. You cannot set your own permissions level.
    --
    -- -   @deny@
    --
    -- -   @show@
    --
    -- -   @deploy@
    --
    -- -   @manage@
    --
    -- -   @iam_only@
    --
    -- For more information about the permissions associated with these levels,
    -- see
    -- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
    level :: Prelude.Maybe Prelude.Text,
    -- | The stack ID.
    stackId :: Prelude.Text,
    -- | The user\'s IAM ARN. This can also be a federated user\'s ARN.
    iamUserArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowSsh', 'setPermission_allowSsh' - The user is allowed to use SSH to communicate with the instance.
--
-- 'allowSudo', 'setPermission_allowSudo' - The user is allowed to use __sudo__ to elevate privileges.
--
-- 'level', 'setPermission_level' - The user\'s permission level, which must be set to one of the following
-- strings. You cannot set your own permissions level.
--
-- -   @deny@
--
-- -   @show@
--
-- -   @deploy@
--
-- -   @manage@
--
-- -   @iam_only@
--
-- For more information about the permissions associated with these levels,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- 'stackId', 'setPermission_stackId' - The stack ID.
--
-- 'iamUserArn', 'setPermission_iamUserArn' - The user\'s IAM ARN. This can also be a federated user\'s ARN.
newSetPermission ::
  -- | 'stackId'
  Prelude.Text ->
  -- | 'iamUserArn'
  Prelude.Text ->
  SetPermission
newSetPermission pStackId_ pIamUserArn_ =
  SetPermission'
    { allowSsh = Prelude.Nothing,
      allowSudo = Prelude.Nothing,
      level = Prelude.Nothing,
      stackId = pStackId_,
      iamUserArn = pIamUserArn_
    }

-- | The user is allowed to use SSH to communicate with the instance.
setPermission_allowSsh :: Lens.Lens' SetPermission (Prelude.Maybe Prelude.Bool)
setPermission_allowSsh = Lens.lens (\SetPermission' {allowSsh} -> allowSsh) (\s@SetPermission' {} a -> s {allowSsh = a} :: SetPermission)

-- | The user is allowed to use __sudo__ to elevate privileges.
setPermission_allowSudo :: Lens.Lens' SetPermission (Prelude.Maybe Prelude.Bool)
setPermission_allowSudo = Lens.lens (\SetPermission' {allowSudo} -> allowSudo) (\s@SetPermission' {} a -> s {allowSudo = a} :: SetPermission)

-- | The user\'s permission level, which must be set to one of the following
-- strings. You cannot set your own permissions level.
--
-- -   @deny@
--
-- -   @show@
--
-- -   @deploy@
--
-- -   @manage@
--
-- -   @iam_only@
--
-- For more information about the permissions associated with these levels,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
setPermission_level :: Lens.Lens' SetPermission (Prelude.Maybe Prelude.Text)
setPermission_level = Lens.lens (\SetPermission' {level} -> level) (\s@SetPermission' {} a -> s {level = a} :: SetPermission)

-- | The stack ID.
setPermission_stackId :: Lens.Lens' SetPermission Prelude.Text
setPermission_stackId = Lens.lens (\SetPermission' {stackId} -> stackId) (\s@SetPermission' {} a -> s {stackId = a} :: SetPermission)

-- | The user\'s IAM ARN. This can also be a federated user\'s ARN.
setPermission_iamUserArn :: Lens.Lens' SetPermission Prelude.Text
setPermission_iamUserArn = Lens.lens (\SetPermission' {iamUserArn} -> iamUserArn) (\s@SetPermission' {} a -> s {iamUserArn = a} :: SetPermission)

instance Core.AWSRequest SetPermission where
  type
    AWSResponse SetPermission =
      SetPermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull SetPermissionResponse'

instance Prelude.Hashable SetPermission where
  hashWithSalt _salt SetPermission' {..} =
    _salt `Prelude.hashWithSalt` allowSsh
      `Prelude.hashWithSalt` allowSudo
      `Prelude.hashWithSalt` level
      `Prelude.hashWithSalt` stackId
      `Prelude.hashWithSalt` iamUserArn

instance Prelude.NFData SetPermission where
  rnf SetPermission' {..} =
    Prelude.rnf allowSsh
      `Prelude.seq` Prelude.rnf allowSudo
      `Prelude.seq` Prelude.rnf level
      `Prelude.seq` Prelude.rnf stackId
      `Prelude.seq` Prelude.rnf iamUserArn

instance Data.ToHeaders SetPermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.SetPermission" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SetPermission where
  toJSON SetPermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowSsh" Data..=) Prelude.<$> allowSsh,
            ("AllowSudo" Data..=) Prelude.<$> allowSudo,
            ("Level" Data..=) Prelude.<$> level,
            Prelude.Just ("StackId" Data..= stackId),
            Prelude.Just ("IamUserArn" Data..= iamUserArn)
          ]
      )

instance Data.ToPath SetPermission where
  toPath = Prelude.const "/"

instance Data.ToQuery SetPermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetPermissionResponse' smart constructor.
data SetPermissionResponse = SetPermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetPermissionResponse ::
  SetPermissionResponse
newSetPermissionResponse = SetPermissionResponse'

instance Prelude.NFData SetPermissionResponse where
  rnf _ = ()
