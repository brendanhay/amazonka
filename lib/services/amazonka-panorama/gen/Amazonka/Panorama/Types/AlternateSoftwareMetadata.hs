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
-- Module      : Amazonka.Panorama.Types.AlternateSoftwareMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.AlternateSoftwareMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a beta appliance software update.
--
-- /See:/ 'newAlternateSoftwareMetadata' smart constructor.
data AlternateSoftwareMetadata = AlternateSoftwareMetadata'
  { -- | The appliance software version.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlternateSoftwareMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'alternateSoftwareMetadata_version' - The appliance software version.
newAlternateSoftwareMetadata ::
  AlternateSoftwareMetadata
newAlternateSoftwareMetadata =
  AlternateSoftwareMetadata'
    { version =
        Prelude.Nothing
    }

-- | The appliance software version.
alternateSoftwareMetadata_version :: Lens.Lens' AlternateSoftwareMetadata (Prelude.Maybe Prelude.Text)
alternateSoftwareMetadata_version = Lens.lens (\AlternateSoftwareMetadata' {version} -> version) (\s@AlternateSoftwareMetadata' {} a -> s {version = a} :: AlternateSoftwareMetadata)

instance Data.FromJSON AlternateSoftwareMetadata where
  parseJSON =
    Data.withObject
      "AlternateSoftwareMetadata"
      ( \x ->
          AlternateSoftwareMetadata'
            Prelude.<$> (x Data..:? "Version")
      )

instance Prelude.Hashable AlternateSoftwareMetadata where
  hashWithSalt _salt AlternateSoftwareMetadata' {..} =
    _salt `Prelude.hashWithSalt` version

instance Prelude.NFData AlternateSoftwareMetadata where
  rnf AlternateSoftwareMetadata' {..} =
    Prelude.rnf version
