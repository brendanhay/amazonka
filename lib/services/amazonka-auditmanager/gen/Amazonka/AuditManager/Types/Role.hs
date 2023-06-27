{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AuditManager.Types.Role
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Role where

import Amazonka.AuditManager.Types.RoleType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The wrapper that contains the Audit Manager role information of the
-- current user. This includes the role type and IAM Amazon Resource Name
-- (ARN).
--
-- /See:/ 'newRole' smart constructor.
data Role = Role'
  { -- | The type of customer persona.
    --
    -- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
    --
    -- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
    --
    -- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
    -- @RESOURCE_OWNER@.
    roleType :: RoleType,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Role' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleType', 'role_roleType' - The type of customer persona.
--
-- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
-- @RESOURCE_OWNER@.
--
-- 'roleArn', 'role_roleArn' - The Amazon Resource Name (ARN) of the IAM role.
newRole ::
  -- | 'roleType'
  RoleType ->
  -- | 'roleArn'
  Prelude.Text ->
  Role
newRole pRoleType_ pRoleArn_ =
  Role' {roleType = pRoleType_, roleArn = pRoleArn_}

-- | The type of customer persona.
--
-- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
-- @RESOURCE_OWNER@.
role_roleType :: Lens.Lens' Role RoleType
role_roleType = Lens.lens (\Role' {roleType} -> roleType) (\s@Role' {} a -> s {roleType = a} :: Role)

-- | The Amazon Resource Name (ARN) of the IAM role.
role_roleArn :: Lens.Lens' Role Prelude.Text
role_roleArn = Lens.lens (\Role' {roleArn} -> roleArn) (\s@Role' {} a -> s {roleArn = a} :: Role)

instance Data.FromJSON Role where
  parseJSON =
    Data.withObject
      "Role"
      ( \x ->
          Role'
            Prelude.<$> (x Data..: "roleType")
            Prelude.<*> (x Data..: "roleArn")
      )

instance Prelude.Hashable Role where
  hashWithSalt _salt Role' {..} =
    _salt
      `Prelude.hashWithSalt` roleType
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData Role where
  rnf Role' {..} =
    Prelude.rnf roleType
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToJSON Role where
  toJSON Role' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleType" Data..= roleType),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )
