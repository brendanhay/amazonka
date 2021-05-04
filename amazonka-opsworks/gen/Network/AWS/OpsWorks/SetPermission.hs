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
-- Module      : Network.AWS.OpsWorks.SetPermission
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.SetPermission
  ( -- * Creating a Request
    SetPermission (..),
    newSetPermission,

    -- * Request Lenses
    setPermission_allowSudo,
    setPermission_allowSsh,
    setPermission_level,
    setPermission_stackId,
    setPermission_iamUserArn,

    -- * Destructuring the Response
    SetPermissionResponse (..),
    newSetPermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetPermission' smart constructor.
data SetPermission = SetPermission'
  { -- | The user is allowed to use __sudo__ to elevate privileges.
    allowSudo :: Prelude.Maybe Prelude.Bool,
    -- | The user is allowed to use SSH to communicate with the instance.
    allowSsh :: Prelude.Maybe Prelude.Bool,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowSudo', 'setPermission_allowSudo' - The user is allowed to use __sudo__ to elevate privileges.
--
-- 'allowSsh', 'setPermission_allowSsh' - The user is allowed to use SSH to communicate with the instance.
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
    { allowSudo = Prelude.Nothing,
      allowSsh = Prelude.Nothing,
      level = Prelude.Nothing,
      stackId = pStackId_,
      iamUserArn = pIamUserArn_
    }

-- | The user is allowed to use __sudo__ to elevate privileges.
setPermission_allowSudo :: Lens.Lens' SetPermission (Prelude.Maybe Prelude.Bool)
setPermission_allowSudo = Lens.lens (\SetPermission' {allowSudo} -> allowSudo) (\s@SetPermission' {} a -> s {allowSudo = a} :: SetPermission)

-- | The user is allowed to use SSH to communicate with the instance.
setPermission_allowSsh :: Lens.Lens' SetPermission (Prelude.Maybe Prelude.Bool)
setPermission_allowSsh = Lens.lens (\SetPermission' {allowSsh} -> allowSsh) (\s@SetPermission' {} a -> s {allowSsh = a} :: SetPermission)

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

instance Prelude.AWSRequest SetPermission where
  type Rs SetPermission = SetPermissionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetPermissionResponse'

instance Prelude.Hashable SetPermission

instance Prelude.NFData SetPermission

instance Prelude.ToHeaders SetPermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OpsWorks_20130218.SetPermission" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetPermission where
  toJSON SetPermission' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AllowSudo" Prelude..=) Prelude.<$> allowSudo,
            ("AllowSsh" Prelude..=) Prelude.<$> allowSsh,
            ("Level" Prelude..=) Prelude.<$> level,
            Prelude.Just ("StackId" Prelude..= stackId),
            Prelude.Just ("IamUserArn" Prelude..= iamUserArn)
          ]
      )

instance Prelude.ToPath SetPermission where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetPermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetPermissionResponse' smart constructor.
data SetPermissionResponse = SetPermissionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetPermissionResponse ::
  SetPermissionResponse
newSetPermissionResponse = SetPermissionResponse'

instance Prelude.NFData SetPermissionResponse
