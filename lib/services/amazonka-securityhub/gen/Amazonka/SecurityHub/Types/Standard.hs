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
-- Module      : Amazonka.SecurityHub.Types.Standard
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Standard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StandardsManagedBy

-- | Provides information about a specific security standard.
--
-- /See:/ 'newStandard' smart constructor.
data Standard = Standard'
  { -- | A description of the standard.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether the standard is enabled by default. When Security Hub is enabled
    -- from the console, if a standard is enabled by default, the check box for
    -- that standard is selected by default.
    --
    -- When Security Hub is enabled using the @EnableSecurityHub@ API
    -- operation, the standard is enabled by default unless
    -- @EnableDefaultStandards@ is set to @false@.
    enabledByDefault :: Prelude.Maybe Prelude.Bool,
    -- | The name of the standard.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a standard.
    standardsArn :: Prelude.Maybe Prelude.Text,
    -- | Provides details about the management of a standard.
    standardsManagedBy :: Prelude.Maybe StandardsManagedBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Standard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'standard_description' - A description of the standard.
--
-- 'enabledByDefault', 'standard_enabledByDefault' - Whether the standard is enabled by default. When Security Hub is enabled
-- from the console, if a standard is enabled by default, the check box for
-- that standard is selected by default.
--
-- When Security Hub is enabled using the @EnableSecurityHub@ API
-- operation, the standard is enabled by default unless
-- @EnableDefaultStandards@ is set to @false@.
--
-- 'name', 'standard_name' - The name of the standard.
--
-- 'standardsArn', 'standard_standardsArn' - The ARN of a standard.
--
-- 'standardsManagedBy', 'standard_standardsManagedBy' - Provides details about the management of a standard.
newStandard ::
  Standard
newStandard =
  Standard'
    { description = Prelude.Nothing,
      enabledByDefault = Prelude.Nothing,
      name = Prelude.Nothing,
      standardsArn = Prelude.Nothing,
      standardsManagedBy = Prelude.Nothing
    }

-- | A description of the standard.
standard_description :: Lens.Lens' Standard (Prelude.Maybe Prelude.Text)
standard_description = Lens.lens (\Standard' {description} -> description) (\s@Standard' {} a -> s {description = a} :: Standard)

-- | Whether the standard is enabled by default. When Security Hub is enabled
-- from the console, if a standard is enabled by default, the check box for
-- that standard is selected by default.
--
-- When Security Hub is enabled using the @EnableSecurityHub@ API
-- operation, the standard is enabled by default unless
-- @EnableDefaultStandards@ is set to @false@.
standard_enabledByDefault :: Lens.Lens' Standard (Prelude.Maybe Prelude.Bool)
standard_enabledByDefault = Lens.lens (\Standard' {enabledByDefault} -> enabledByDefault) (\s@Standard' {} a -> s {enabledByDefault = a} :: Standard)

-- | The name of the standard.
standard_name :: Lens.Lens' Standard (Prelude.Maybe Prelude.Text)
standard_name = Lens.lens (\Standard' {name} -> name) (\s@Standard' {} a -> s {name = a} :: Standard)

-- | The ARN of a standard.
standard_standardsArn :: Lens.Lens' Standard (Prelude.Maybe Prelude.Text)
standard_standardsArn = Lens.lens (\Standard' {standardsArn} -> standardsArn) (\s@Standard' {} a -> s {standardsArn = a} :: Standard)

-- | Provides details about the management of a standard.
standard_standardsManagedBy :: Lens.Lens' Standard (Prelude.Maybe StandardsManagedBy)
standard_standardsManagedBy = Lens.lens (\Standard' {standardsManagedBy} -> standardsManagedBy) (\s@Standard' {} a -> s {standardsManagedBy = a} :: Standard)

instance Data.FromJSON Standard where
  parseJSON =
    Data.withObject
      "Standard"
      ( \x ->
          Standard'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EnabledByDefault")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "StandardsArn")
            Prelude.<*> (x Data..:? "StandardsManagedBy")
      )

instance Prelude.Hashable Standard where
  hashWithSalt _salt Standard' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enabledByDefault
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` standardsArn
      `Prelude.hashWithSalt` standardsManagedBy

instance Prelude.NFData Standard where
  rnf Standard' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf enabledByDefault
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf standardsArn
      `Prelude.seq` Prelude.rnf standardsManagedBy
