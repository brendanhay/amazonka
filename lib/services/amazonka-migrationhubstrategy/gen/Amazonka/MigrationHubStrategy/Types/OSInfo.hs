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
-- Module      : Amazonka.MigrationHubStrategy.Types.OSInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.OSInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.OSType
import qualified Amazonka.Prelude as Prelude

-- | Information about the operating system.
--
-- /See:/ 'newOSInfo' smart constructor.
data OSInfo = OSInfo'
  { -- | Information about the type of operating system.
    type' :: Prelude.Maybe OSType,
    -- | Information about the version of operating system.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OSInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'oSInfo_type' - Information about the type of operating system.
--
-- 'version', 'oSInfo_version' - Information about the version of operating system.
newOSInfo ::
  OSInfo
newOSInfo =
  OSInfo'
    { type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | Information about the type of operating system.
oSInfo_type :: Lens.Lens' OSInfo (Prelude.Maybe OSType)
oSInfo_type = Lens.lens (\OSInfo' {type'} -> type') (\s@OSInfo' {} a -> s {type' = a} :: OSInfo)

-- | Information about the version of operating system.
oSInfo_version :: Lens.Lens' OSInfo (Prelude.Maybe Prelude.Text)
oSInfo_version = Lens.lens (\OSInfo' {version} -> version) (\s@OSInfo' {} a -> s {version = a} :: OSInfo)

instance Data.FromJSON OSInfo where
  parseJSON =
    Data.withObject
      "OSInfo"
      ( \x ->
          OSInfo'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable OSInfo where
  hashWithSalt _salt OSInfo' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version

instance Prelude.NFData OSInfo where
  rnf OSInfo' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf version
