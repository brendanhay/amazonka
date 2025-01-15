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
-- Module      : Amazonka.CodeArtifact.Types.PackageVersionError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageVersionError where

import Amazonka.CodeArtifact.Types.PackageVersionErrorCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | l An error associated with package.
--
-- /See:/ 'newPackageVersionError' smart constructor.
data PackageVersionError = PackageVersionError'
  { -- | The error code associated with the error. Valid error codes are:
    --
    -- -   @ALREADY_EXISTS@
    --
    -- -   @MISMATCHED_REVISION@
    --
    -- -   @MISMATCHED_STATUS@
    --
    -- -   @NOT_ALLOWED@
    --
    -- -   @NOT_FOUND@
    --
    -- -   @SKIPPED@
    errorCode :: Prelude.Maybe PackageVersionErrorCode,
    -- | The error message associated with the error.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'packageVersionError_errorCode' - The error code associated with the error. Valid error codes are:
--
-- -   @ALREADY_EXISTS@
--
-- -   @MISMATCHED_REVISION@
--
-- -   @MISMATCHED_STATUS@
--
-- -   @NOT_ALLOWED@
--
-- -   @NOT_FOUND@
--
-- -   @SKIPPED@
--
-- 'errorMessage', 'packageVersionError_errorMessage' - The error message associated with the error.
newPackageVersionError ::
  PackageVersionError
newPackageVersionError =
  PackageVersionError'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The error code associated with the error. Valid error codes are:
--
-- -   @ALREADY_EXISTS@
--
-- -   @MISMATCHED_REVISION@
--
-- -   @MISMATCHED_STATUS@
--
-- -   @NOT_ALLOWED@
--
-- -   @NOT_FOUND@
--
-- -   @SKIPPED@
packageVersionError_errorCode :: Lens.Lens' PackageVersionError (Prelude.Maybe PackageVersionErrorCode)
packageVersionError_errorCode = Lens.lens (\PackageVersionError' {errorCode} -> errorCode) (\s@PackageVersionError' {} a -> s {errorCode = a} :: PackageVersionError)

-- | The error message associated with the error.
packageVersionError_errorMessage :: Lens.Lens' PackageVersionError (Prelude.Maybe Prelude.Text)
packageVersionError_errorMessage = Lens.lens (\PackageVersionError' {errorMessage} -> errorMessage) (\s@PackageVersionError' {} a -> s {errorMessage = a} :: PackageVersionError)

instance Data.FromJSON PackageVersionError where
  parseJSON =
    Data.withObject
      "PackageVersionError"
      ( \x ->
          PackageVersionError'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
      )

instance Prelude.Hashable PackageVersionError where
  hashWithSalt _salt PackageVersionError' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData PackageVersionError where
  rnf PackageVersionError' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorMessage
