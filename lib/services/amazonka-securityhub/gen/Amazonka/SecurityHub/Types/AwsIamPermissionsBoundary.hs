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
-- Module      : Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the policy used to set the permissions boundary for an
-- IAM principal.
--
-- /See:/ 'newAwsIamPermissionsBoundary' smart constructor.
data AwsIamPermissionsBoundary = AwsIamPermissionsBoundary'
  { -- | The usage type for the permissions boundary.
    permissionsBoundaryType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the policy used to set the permissions boundary.
    permissionsBoundaryArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamPermissionsBoundary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionsBoundaryType', 'awsIamPermissionsBoundary_permissionsBoundaryType' - The usage type for the permissions boundary.
--
-- 'permissionsBoundaryArn', 'awsIamPermissionsBoundary_permissionsBoundaryArn' - The ARN of the policy used to set the permissions boundary.
newAwsIamPermissionsBoundary ::
  AwsIamPermissionsBoundary
newAwsIamPermissionsBoundary =
  AwsIamPermissionsBoundary'
    { permissionsBoundaryType =
        Prelude.Nothing,
      permissionsBoundaryArn = Prelude.Nothing
    }

-- | The usage type for the permissions boundary.
awsIamPermissionsBoundary_permissionsBoundaryType :: Lens.Lens' AwsIamPermissionsBoundary (Prelude.Maybe Prelude.Text)
awsIamPermissionsBoundary_permissionsBoundaryType = Lens.lens (\AwsIamPermissionsBoundary' {permissionsBoundaryType} -> permissionsBoundaryType) (\s@AwsIamPermissionsBoundary' {} a -> s {permissionsBoundaryType = a} :: AwsIamPermissionsBoundary)

-- | The ARN of the policy used to set the permissions boundary.
awsIamPermissionsBoundary_permissionsBoundaryArn :: Lens.Lens' AwsIamPermissionsBoundary (Prelude.Maybe Prelude.Text)
awsIamPermissionsBoundary_permissionsBoundaryArn = Lens.lens (\AwsIamPermissionsBoundary' {permissionsBoundaryArn} -> permissionsBoundaryArn) (\s@AwsIamPermissionsBoundary' {} a -> s {permissionsBoundaryArn = a} :: AwsIamPermissionsBoundary)

instance Core.FromJSON AwsIamPermissionsBoundary where
  parseJSON =
    Core.withObject
      "AwsIamPermissionsBoundary"
      ( \x ->
          AwsIamPermissionsBoundary'
            Prelude.<$> (x Core..:? "PermissionsBoundaryType")
            Prelude.<*> (x Core..:? "PermissionsBoundaryArn")
      )

instance Prelude.Hashable AwsIamPermissionsBoundary where
  hashWithSalt _salt AwsIamPermissionsBoundary' {..} =
    _salt
      `Prelude.hashWithSalt` permissionsBoundaryType
      `Prelude.hashWithSalt` permissionsBoundaryArn

instance Prelude.NFData AwsIamPermissionsBoundary where
  rnf AwsIamPermissionsBoundary' {..} =
    Prelude.rnf permissionsBoundaryType
      `Prelude.seq` Prelude.rnf permissionsBoundaryArn

instance Core.ToJSON AwsIamPermissionsBoundary where
  toJSON AwsIamPermissionsBoundary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PermissionsBoundaryType" Core..=)
              Prelude.<$> permissionsBoundaryType,
            ("PermissionsBoundaryArn" Core..=)
              Prelude.<$> permissionsBoundaryArn
          ]
      )
