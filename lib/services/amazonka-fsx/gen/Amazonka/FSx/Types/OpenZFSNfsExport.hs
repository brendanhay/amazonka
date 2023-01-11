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
-- Module      : Amazonka.FSx.Types.OpenZFSNfsExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OpenZFSNfsExport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.OpenZFSClientConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The Network File System (NFS) configurations for mounting an Amazon FSx
-- for OpenZFS file system.
--
-- /See:/ 'newOpenZFSNfsExport' smart constructor.
data OpenZFSNfsExport = OpenZFSNfsExport'
  { -- | A list of configuration objects that contain the client and options for
    -- mounting the OpenZFS file system.
    clientConfigurations :: [OpenZFSClientConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenZFSNfsExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientConfigurations', 'openZFSNfsExport_clientConfigurations' - A list of configuration objects that contain the client and options for
-- mounting the OpenZFS file system.
newOpenZFSNfsExport ::
  OpenZFSNfsExport
newOpenZFSNfsExport =
  OpenZFSNfsExport'
    { clientConfigurations =
        Prelude.mempty
    }

-- | A list of configuration objects that contain the client and options for
-- mounting the OpenZFS file system.
openZFSNfsExport_clientConfigurations :: Lens.Lens' OpenZFSNfsExport [OpenZFSClientConfiguration]
openZFSNfsExport_clientConfigurations = Lens.lens (\OpenZFSNfsExport' {clientConfigurations} -> clientConfigurations) (\s@OpenZFSNfsExport' {} a -> s {clientConfigurations = a} :: OpenZFSNfsExport) Prelude.. Lens.coerced

instance Data.FromJSON OpenZFSNfsExport where
  parseJSON =
    Data.withObject
      "OpenZFSNfsExport"
      ( \x ->
          OpenZFSNfsExport'
            Prelude.<$> ( x Data..:? "ClientConfigurations"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable OpenZFSNfsExport where
  hashWithSalt _salt OpenZFSNfsExport' {..} =
    _salt `Prelude.hashWithSalt` clientConfigurations

instance Prelude.NFData OpenZFSNfsExport where
  rnf OpenZFSNfsExport' {..} =
    Prelude.rnf clientConfigurations

instance Data.ToJSON OpenZFSNfsExport where
  toJSON OpenZFSNfsExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ClientConfigurations"
                  Data..= clientConfigurations
              )
          ]
      )
