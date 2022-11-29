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
-- Module      : Amazonka.GuardDuty.Types.PublicAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.PublicAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.PermissionConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the public access policies that apply to the S3 bucket.
--
-- /See:/ 'newPublicAccess' smart constructor.
data PublicAccess = PublicAccess'
  { -- | Contains information about how permissions are configured for the S3
    -- bucket.
    permissionConfiguration :: Prelude.Maybe PermissionConfiguration,
    -- | Describes the effective permission on this bucket after factoring all
    -- attached policies.
    effectivePermission :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublicAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'permissionConfiguration', 'publicAccess_permissionConfiguration' - Contains information about how permissions are configured for the S3
-- bucket.
--
-- 'effectivePermission', 'publicAccess_effectivePermission' - Describes the effective permission on this bucket after factoring all
-- attached policies.
newPublicAccess ::
  PublicAccess
newPublicAccess =
  PublicAccess'
    { permissionConfiguration =
        Prelude.Nothing,
      effectivePermission = Prelude.Nothing
    }

-- | Contains information about how permissions are configured for the S3
-- bucket.
publicAccess_permissionConfiguration :: Lens.Lens' PublicAccess (Prelude.Maybe PermissionConfiguration)
publicAccess_permissionConfiguration = Lens.lens (\PublicAccess' {permissionConfiguration} -> permissionConfiguration) (\s@PublicAccess' {} a -> s {permissionConfiguration = a} :: PublicAccess)

-- | Describes the effective permission on this bucket after factoring all
-- attached policies.
publicAccess_effectivePermission :: Lens.Lens' PublicAccess (Prelude.Maybe Prelude.Text)
publicAccess_effectivePermission = Lens.lens (\PublicAccess' {effectivePermission} -> effectivePermission) (\s@PublicAccess' {} a -> s {effectivePermission = a} :: PublicAccess)

instance Core.FromJSON PublicAccess where
  parseJSON =
    Core.withObject
      "PublicAccess"
      ( \x ->
          PublicAccess'
            Prelude.<$> (x Core..:? "permissionConfiguration")
            Prelude.<*> (x Core..:? "effectivePermission")
      )

instance Prelude.Hashable PublicAccess where
  hashWithSalt _salt PublicAccess' {..} =
    _salt
      `Prelude.hashWithSalt` permissionConfiguration
      `Prelude.hashWithSalt` effectivePermission

instance Prelude.NFData PublicAccess where
  rnf PublicAccess' {..} =
    Prelude.rnf permissionConfiguration
      `Prelude.seq` Prelude.rnf effectivePermission
