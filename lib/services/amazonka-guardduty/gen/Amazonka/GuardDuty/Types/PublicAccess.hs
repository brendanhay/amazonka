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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.PublicAccess where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.PermissionConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Describes the public access policies that apply to the S3 bucket.
--
-- /See:/ 'newPublicAccess' smart constructor.
data PublicAccess = PublicAccess'
  { -- | Describes the effective permission on this bucket after factoring all
    -- attached policies.
    effectivePermission :: Prelude.Maybe Prelude.Text,
    -- | Contains information about how permissions are configured for the S3
    -- bucket.
    permissionConfiguration :: Prelude.Maybe PermissionConfiguration
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
-- 'effectivePermission', 'publicAccess_effectivePermission' - Describes the effective permission on this bucket after factoring all
-- attached policies.
--
-- 'permissionConfiguration', 'publicAccess_permissionConfiguration' - Contains information about how permissions are configured for the S3
-- bucket.
newPublicAccess ::
  PublicAccess
newPublicAccess =
  PublicAccess'
    { effectivePermission =
        Prelude.Nothing,
      permissionConfiguration = Prelude.Nothing
    }

-- | Describes the effective permission on this bucket after factoring all
-- attached policies.
publicAccess_effectivePermission :: Lens.Lens' PublicAccess (Prelude.Maybe Prelude.Text)
publicAccess_effectivePermission = Lens.lens (\PublicAccess' {effectivePermission} -> effectivePermission) (\s@PublicAccess' {} a -> s {effectivePermission = a} :: PublicAccess)

-- | Contains information about how permissions are configured for the S3
-- bucket.
publicAccess_permissionConfiguration :: Lens.Lens' PublicAccess (Prelude.Maybe PermissionConfiguration)
publicAccess_permissionConfiguration = Lens.lens (\PublicAccess' {permissionConfiguration} -> permissionConfiguration) (\s@PublicAccess' {} a -> s {permissionConfiguration = a} :: PublicAccess)

instance Data.FromJSON PublicAccess where
  parseJSON =
    Data.withObject
      "PublicAccess"
      ( \x ->
          PublicAccess'
            Prelude.<$> (x Data..:? "effectivePermission")
            Prelude.<*> (x Data..:? "permissionConfiguration")
      )

instance Prelude.Hashable PublicAccess where
  hashWithSalt _salt PublicAccess' {..} =
    _salt
      `Prelude.hashWithSalt` effectivePermission
      `Prelude.hashWithSalt` permissionConfiguration

instance Prelude.NFData PublicAccess where
  rnf PublicAccess' {..} =
    Prelude.rnf effectivePermission
      `Prelude.seq` Prelude.rnf permissionConfiguration
