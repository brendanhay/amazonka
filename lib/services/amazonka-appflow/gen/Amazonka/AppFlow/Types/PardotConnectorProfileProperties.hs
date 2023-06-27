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
-- Module      : Amazonka.AppFlow.Types.PardotConnectorProfileProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PardotConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using Salesforce
-- Pardot.
--
-- /See:/ 'newPardotConnectorProfileProperties' smart constructor.
data PardotConnectorProfileProperties = PardotConnectorProfileProperties'
  { -- | The business unit id of Salesforce Pardot instance.
    businessUnitId :: Prelude.Maybe Prelude.Text,
    -- | The location of the Salesforce Pardot resource.
    instanceUrl :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the connector profile applies to a sandbox or
    -- production environment.
    isSandboxEnvironment :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PardotConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'businessUnitId', 'pardotConnectorProfileProperties_businessUnitId' - The business unit id of Salesforce Pardot instance.
--
-- 'instanceUrl', 'pardotConnectorProfileProperties_instanceUrl' - The location of the Salesforce Pardot resource.
--
-- 'isSandboxEnvironment', 'pardotConnectorProfileProperties_isSandboxEnvironment' - Indicates whether the connector profile applies to a sandbox or
-- production environment.
newPardotConnectorProfileProperties ::
  PardotConnectorProfileProperties
newPardotConnectorProfileProperties =
  PardotConnectorProfileProperties'
    { businessUnitId =
        Prelude.Nothing,
      instanceUrl = Prelude.Nothing,
      isSandboxEnvironment = Prelude.Nothing
    }

-- | The business unit id of Salesforce Pardot instance.
pardotConnectorProfileProperties_businessUnitId :: Lens.Lens' PardotConnectorProfileProperties (Prelude.Maybe Prelude.Text)
pardotConnectorProfileProperties_businessUnitId = Lens.lens (\PardotConnectorProfileProperties' {businessUnitId} -> businessUnitId) (\s@PardotConnectorProfileProperties' {} a -> s {businessUnitId = a} :: PardotConnectorProfileProperties)

-- | The location of the Salesforce Pardot resource.
pardotConnectorProfileProperties_instanceUrl :: Lens.Lens' PardotConnectorProfileProperties (Prelude.Maybe Prelude.Text)
pardotConnectorProfileProperties_instanceUrl = Lens.lens (\PardotConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@PardotConnectorProfileProperties' {} a -> s {instanceUrl = a} :: PardotConnectorProfileProperties)

-- | Indicates whether the connector profile applies to a sandbox or
-- production environment.
pardotConnectorProfileProperties_isSandboxEnvironment :: Lens.Lens' PardotConnectorProfileProperties (Prelude.Maybe Prelude.Bool)
pardotConnectorProfileProperties_isSandboxEnvironment = Lens.lens (\PardotConnectorProfileProperties' {isSandboxEnvironment} -> isSandboxEnvironment) (\s@PardotConnectorProfileProperties' {} a -> s {isSandboxEnvironment = a} :: PardotConnectorProfileProperties)

instance
  Data.FromJSON
    PardotConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "PardotConnectorProfileProperties"
      ( \x ->
          PardotConnectorProfileProperties'
            Prelude.<$> (x Data..:? "businessUnitId")
            Prelude.<*> (x Data..:? "instanceUrl")
            Prelude.<*> (x Data..:? "isSandboxEnvironment")
      )

instance
  Prelude.Hashable
    PardotConnectorProfileProperties
  where
  hashWithSalt
    _salt
    PardotConnectorProfileProperties' {..} =
      _salt
        `Prelude.hashWithSalt` businessUnitId
        `Prelude.hashWithSalt` instanceUrl
        `Prelude.hashWithSalt` isSandboxEnvironment

instance
  Prelude.NFData
    PardotConnectorProfileProperties
  where
  rnf PardotConnectorProfileProperties' {..} =
    Prelude.rnf businessUnitId
      `Prelude.seq` Prelude.rnf instanceUrl
      `Prelude.seq` Prelude.rnf isSandboxEnvironment

instance Data.ToJSON PardotConnectorProfileProperties where
  toJSON PardotConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("businessUnitId" Data..=)
              Prelude.<$> businessUnitId,
            ("instanceUrl" Data..=) Prelude.<$> instanceUrl,
            ("isSandboxEnvironment" Data..=)
              Prelude.<$> isSandboxEnvironment
          ]
      )
