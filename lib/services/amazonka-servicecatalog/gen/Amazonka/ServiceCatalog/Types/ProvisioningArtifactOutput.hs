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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningArtifactOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningArtifactOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provisioning artifact output.
--
-- /See:/ 'newProvisioningArtifactOutput' smart constructor.
data ProvisioningArtifactOutput = ProvisioningArtifactOutput'
  { -- | Description of the provisioning artifact output key.
    description :: Prelude.Maybe Prelude.Text,
    -- | The provisioning artifact output key.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningArtifactOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'provisioningArtifactOutput_description' - Description of the provisioning artifact output key.
--
-- 'key', 'provisioningArtifactOutput_key' - The provisioning artifact output key.
newProvisioningArtifactOutput ::
  ProvisioningArtifactOutput
newProvisioningArtifactOutput =
  ProvisioningArtifactOutput'
    { description =
        Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | Description of the provisioning artifact output key.
provisioningArtifactOutput_description :: Lens.Lens' ProvisioningArtifactOutput (Prelude.Maybe Prelude.Text)
provisioningArtifactOutput_description = Lens.lens (\ProvisioningArtifactOutput' {description} -> description) (\s@ProvisioningArtifactOutput' {} a -> s {description = a} :: ProvisioningArtifactOutput)

-- | The provisioning artifact output key.
provisioningArtifactOutput_key :: Lens.Lens' ProvisioningArtifactOutput (Prelude.Maybe Prelude.Text)
provisioningArtifactOutput_key = Lens.lens (\ProvisioningArtifactOutput' {key} -> key) (\s@ProvisioningArtifactOutput' {} a -> s {key = a} :: ProvisioningArtifactOutput)

instance Data.FromJSON ProvisioningArtifactOutput where
  parseJSON =
    Data.withObject
      "ProvisioningArtifactOutput"
      ( \x ->
          ProvisioningArtifactOutput'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Key")
      )

instance Prelude.Hashable ProvisioningArtifactOutput where
  hashWithSalt _salt ProvisioningArtifactOutput' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` key

instance Prelude.NFData ProvisioningArtifactOutput where
  rnf ProvisioningArtifactOutput' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf key
