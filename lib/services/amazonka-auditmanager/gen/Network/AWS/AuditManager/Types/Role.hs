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
-- Module      : Network.AWS.AuditManager.Types.Role
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.Role where

import Network.AWS.AuditManager.Types.RoleType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The wrapper that contains the Audit Manager role information of the
-- current user, such as the role type and IAM Amazon Resource Name (ARN).
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
    roleType :: Prelude.Maybe RoleType,
    -- | The Amazon Resource Name (ARN) of the IAM role.
    roleArn :: Prelude.Maybe Prelude.Text
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
  Role
newRole =
  Role'
    { roleType = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The type of customer persona.
--
-- In @CreateAssessment@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @UpdateSettings@, @roleType@ can only be @PROCESS_OWNER@.
--
-- In @BatchCreateDelegationByAssessment@, @roleType@ can only be
-- @RESOURCE_OWNER@.
role_roleType :: Lens.Lens' Role (Prelude.Maybe RoleType)
role_roleType = Lens.lens (\Role' {roleType} -> roleType) (\s@Role' {} a -> s {roleType = a} :: Role)

-- | The Amazon Resource Name (ARN) of the IAM role.
role_roleArn :: Lens.Lens' Role (Prelude.Maybe Prelude.Text)
role_roleArn = Lens.lens (\Role' {roleArn} -> roleArn) (\s@Role' {} a -> s {roleArn = a} :: Role)

instance Core.FromJSON Role where
  parseJSON =
    Core.withObject
      "Role"
      ( \x ->
          Role'
            Prelude.<$> (x Core..:? "roleType")
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable Role

instance Prelude.NFData Role

instance Core.ToJSON Role where
  toJSON Role' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("roleType" Core..=) Prelude.<$> roleType,
            ("roleArn" Core..=) Prelude.<$> roleArn
          ]
      )
