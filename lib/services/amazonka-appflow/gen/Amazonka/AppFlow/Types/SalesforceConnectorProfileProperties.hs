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
-- Module      : Amazonka.AppFlow.Types.SalesforceConnectorProfileProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using
-- Salesforce.
--
-- /See:/ 'newSalesforceConnectorProfileProperties' smart constructor.
data SalesforceConnectorProfileProperties = SalesforceConnectorProfileProperties'
  { -- | Indicates whether the connector profile applies to a sandbox or
    -- production environment.
    isSandboxEnvironment :: Prelude.Maybe Prelude.Bool,
    -- | The location of the Salesforce resource.
    instanceUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SalesforceConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isSandboxEnvironment', 'salesforceConnectorProfileProperties_isSandboxEnvironment' - Indicates whether the connector profile applies to a sandbox or
-- production environment.
--
-- 'instanceUrl', 'salesforceConnectorProfileProperties_instanceUrl' - The location of the Salesforce resource.
newSalesforceConnectorProfileProperties ::
  SalesforceConnectorProfileProperties
newSalesforceConnectorProfileProperties =
  SalesforceConnectorProfileProperties'
    { isSandboxEnvironment =
        Prelude.Nothing,
      instanceUrl = Prelude.Nothing
    }

-- | Indicates whether the connector profile applies to a sandbox or
-- production environment.
salesforceConnectorProfileProperties_isSandboxEnvironment :: Lens.Lens' SalesforceConnectorProfileProperties (Prelude.Maybe Prelude.Bool)
salesforceConnectorProfileProperties_isSandboxEnvironment = Lens.lens (\SalesforceConnectorProfileProperties' {isSandboxEnvironment} -> isSandboxEnvironment) (\s@SalesforceConnectorProfileProperties' {} a -> s {isSandboxEnvironment = a} :: SalesforceConnectorProfileProperties)

-- | The location of the Salesforce resource.
salesforceConnectorProfileProperties_instanceUrl :: Lens.Lens' SalesforceConnectorProfileProperties (Prelude.Maybe Prelude.Text)
salesforceConnectorProfileProperties_instanceUrl = Lens.lens (\SalesforceConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@SalesforceConnectorProfileProperties' {} a -> s {instanceUrl = a} :: SalesforceConnectorProfileProperties)

instance
  Core.FromJSON
    SalesforceConnectorProfileProperties
  where
  parseJSON =
    Core.withObject
      "SalesforceConnectorProfileProperties"
      ( \x ->
          SalesforceConnectorProfileProperties'
            Prelude.<$> (x Core..:? "isSandboxEnvironment")
            Prelude.<*> (x Core..:? "instanceUrl")
      )

instance
  Prelude.Hashable
    SalesforceConnectorProfileProperties
  where
  hashWithSalt
    _salt
    SalesforceConnectorProfileProperties' {..} =
      _salt `Prelude.hashWithSalt` isSandboxEnvironment
        `Prelude.hashWithSalt` instanceUrl

instance
  Prelude.NFData
    SalesforceConnectorProfileProperties
  where
  rnf SalesforceConnectorProfileProperties' {..} =
    Prelude.rnf isSandboxEnvironment
      `Prelude.seq` Prelude.rnf instanceUrl

instance
  Core.ToJSON
    SalesforceConnectorProfileProperties
  where
  toJSON SalesforceConnectorProfileProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("isSandboxEnvironment" Core..=)
              Prelude.<$> isSandboxEnvironment,
            ("instanceUrl" Core..=) Prelude.<$> instanceUrl
          ]
      )
