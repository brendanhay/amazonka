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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the policy used to set the permissions boundary for an
-- IAM principal.
--
-- /See:/ 'newAwsIamPermissionsBoundary' smart constructor.
data AwsIamPermissionsBoundary = AwsIamPermissionsBoundary'
  { -- | The ARN of the policy used to set the permissions boundary.
    permissionsBoundaryArn :: Prelude.Maybe Prelude.Text,
    -- | The usage type for the permissions boundary.
    permissionsBoundaryType :: Prelude.Maybe Prelude.Text
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
-- 'permissionsBoundaryArn', 'awsIamPermissionsBoundary_permissionsBoundaryArn' - The ARN of the policy used to set the permissions boundary.
--
-- 'permissionsBoundaryType', 'awsIamPermissionsBoundary_permissionsBoundaryType' - The usage type for the permissions boundary.
newAwsIamPermissionsBoundary ::
  AwsIamPermissionsBoundary
newAwsIamPermissionsBoundary =
  AwsIamPermissionsBoundary'
    { permissionsBoundaryArn =
        Prelude.Nothing,
      permissionsBoundaryType = Prelude.Nothing
    }

-- | The ARN of the policy used to set the permissions boundary.
awsIamPermissionsBoundary_permissionsBoundaryArn :: Lens.Lens' AwsIamPermissionsBoundary (Prelude.Maybe Prelude.Text)
awsIamPermissionsBoundary_permissionsBoundaryArn = Lens.lens (\AwsIamPermissionsBoundary' {permissionsBoundaryArn} -> permissionsBoundaryArn) (\s@AwsIamPermissionsBoundary' {} a -> s {permissionsBoundaryArn = a} :: AwsIamPermissionsBoundary)

-- | The usage type for the permissions boundary.
awsIamPermissionsBoundary_permissionsBoundaryType :: Lens.Lens' AwsIamPermissionsBoundary (Prelude.Maybe Prelude.Text)
awsIamPermissionsBoundary_permissionsBoundaryType = Lens.lens (\AwsIamPermissionsBoundary' {permissionsBoundaryType} -> permissionsBoundaryType) (\s@AwsIamPermissionsBoundary' {} a -> s {permissionsBoundaryType = a} :: AwsIamPermissionsBoundary)

instance Data.FromJSON AwsIamPermissionsBoundary where
  parseJSON =
    Data.withObject
      "AwsIamPermissionsBoundary"
      ( \x ->
          AwsIamPermissionsBoundary'
            Prelude.<$> (x Data..:? "PermissionsBoundaryArn")
            Prelude.<*> (x Data..:? "PermissionsBoundaryType")
      )

instance Prelude.Hashable AwsIamPermissionsBoundary where
  hashWithSalt _salt AwsIamPermissionsBoundary' {..} =
    _salt `Prelude.hashWithSalt` permissionsBoundaryArn
      `Prelude.hashWithSalt` permissionsBoundaryType

instance Prelude.NFData AwsIamPermissionsBoundary where
  rnf AwsIamPermissionsBoundary' {..} =
    Prelude.rnf permissionsBoundaryArn
      `Prelude.seq` Prelude.rnf permissionsBoundaryType

instance Data.ToJSON AwsIamPermissionsBoundary where
  toJSON AwsIamPermissionsBoundary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PermissionsBoundaryArn" Data..=)
              Prelude.<$> permissionsBoundaryArn,
            ("PermissionsBoundaryType" Data..=)
              Prelude.<$> permissionsBoundaryType
          ]
      )
