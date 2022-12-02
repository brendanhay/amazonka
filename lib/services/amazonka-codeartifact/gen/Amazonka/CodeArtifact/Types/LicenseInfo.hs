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
-- Module      : Amazonka.CodeArtifact.Types.LicenseInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.LicenseInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details of the license data.
--
-- /See:/ 'newLicenseInfo' smart constructor.
data LicenseInfo = LicenseInfo'
  { -- | Name of the license.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URL for license data.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LicenseInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'licenseInfo_name' - Name of the license.
--
-- 'url', 'licenseInfo_url' - The URL for license data.
newLicenseInfo ::
  LicenseInfo
newLicenseInfo =
  LicenseInfo'
    { name = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | Name of the license.
licenseInfo_name :: Lens.Lens' LicenseInfo (Prelude.Maybe Prelude.Text)
licenseInfo_name = Lens.lens (\LicenseInfo' {name} -> name) (\s@LicenseInfo' {} a -> s {name = a} :: LicenseInfo)

-- | The URL for license data.
licenseInfo_url :: Lens.Lens' LicenseInfo (Prelude.Maybe Prelude.Text)
licenseInfo_url = Lens.lens (\LicenseInfo' {url} -> url) (\s@LicenseInfo' {} a -> s {url = a} :: LicenseInfo)

instance Data.FromJSON LicenseInfo where
  parseJSON =
    Data.withObject
      "LicenseInfo"
      ( \x ->
          LicenseInfo'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable LicenseInfo where
  hashWithSalt _salt LicenseInfo' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` url

instance Prelude.NFData LicenseInfo where
  rnf LicenseInfo' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf url
