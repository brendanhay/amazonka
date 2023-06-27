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
-- Module      : Amazonka.RDS.Types.OptionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.OptionVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The version for an option. Option group option versions are returned by
-- the @DescribeOptionGroupOptions@ action.
--
-- /See:/ 'newOptionVersion' smart constructor.
data OptionVersion = OptionVersion'
  { -- | True if the version is the default version of the option, and otherwise
    -- false.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The version of the option.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefault', 'optionVersion_isDefault' - True if the version is the default version of the option, and otherwise
-- false.
--
-- 'version', 'optionVersion_version' - The version of the option.
newOptionVersion ::
  OptionVersion
newOptionVersion =
  OptionVersion'
    { isDefault = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | True if the version is the default version of the option, and otherwise
-- false.
optionVersion_isDefault :: Lens.Lens' OptionVersion (Prelude.Maybe Prelude.Bool)
optionVersion_isDefault = Lens.lens (\OptionVersion' {isDefault} -> isDefault) (\s@OptionVersion' {} a -> s {isDefault = a} :: OptionVersion)

-- | The version of the option.
optionVersion_version :: Lens.Lens' OptionVersion (Prelude.Maybe Prelude.Text)
optionVersion_version = Lens.lens (\OptionVersion' {version} -> version) (\s@OptionVersion' {} a -> s {version = a} :: OptionVersion)

instance Data.FromXML OptionVersion where
  parseXML x =
    OptionVersion'
      Prelude.<$> (x Data..@? "IsDefault")
      Prelude.<*> (x Data..@? "Version")

instance Prelude.Hashable OptionVersion where
  hashWithSalt _salt OptionVersion' {..} =
    _salt
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` version

instance Prelude.NFData OptionVersion where
  rnf OptionVersion' {..} =
    Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf version
