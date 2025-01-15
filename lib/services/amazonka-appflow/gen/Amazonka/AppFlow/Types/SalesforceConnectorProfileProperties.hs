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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required when using
-- Salesforce.
--
-- /See:/ 'newSalesforceConnectorProfileProperties' smart constructor.
data SalesforceConnectorProfileProperties = SalesforceConnectorProfileProperties'
  { -- | The location of the Salesforce resource.
    instanceUrl :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the connector profile applies to a sandbox or
    -- production environment.
    isSandboxEnvironment :: Prelude.Maybe Prelude.Bool
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
-- 'instanceUrl', 'salesforceConnectorProfileProperties_instanceUrl' - The location of the Salesforce resource.
--
-- 'isSandboxEnvironment', 'salesforceConnectorProfileProperties_isSandboxEnvironment' - Indicates whether the connector profile applies to a sandbox or
-- production environment.
newSalesforceConnectorProfileProperties ::
  SalesforceConnectorProfileProperties
newSalesforceConnectorProfileProperties =
  SalesforceConnectorProfileProperties'
    { instanceUrl =
        Prelude.Nothing,
      isSandboxEnvironment =
        Prelude.Nothing
    }

-- | The location of the Salesforce resource.
salesforceConnectorProfileProperties_instanceUrl :: Lens.Lens' SalesforceConnectorProfileProperties (Prelude.Maybe Prelude.Text)
salesforceConnectorProfileProperties_instanceUrl = Lens.lens (\SalesforceConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@SalesforceConnectorProfileProperties' {} a -> s {instanceUrl = a} :: SalesforceConnectorProfileProperties)

-- | Indicates whether the connector profile applies to a sandbox or
-- production environment.
salesforceConnectorProfileProperties_isSandboxEnvironment :: Lens.Lens' SalesforceConnectorProfileProperties (Prelude.Maybe Prelude.Bool)
salesforceConnectorProfileProperties_isSandboxEnvironment = Lens.lens (\SalesforceConnectorProfileProperties' {isSandboxEnvironment} -> isSandboxEnvironment) (\s@SalesforceConnectorProfileProperties' {} a -> s {isSandboxEnvironment = a} :: SalesforceConnectorProfileProperties)

instance
  Data.FromJSON
    SalesforceConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "SalesforceConnectorProfileProperties"
      ( \x ->
          SalesforceConnectorProfileProperties'
            Prelude.<$> (x Data..:? "instanceUrl")
            Prelude.<*> (x Data..:? "isSandboxEnvironment")
      )

instance
  Prelude.Hashable
    SalesforceConnectorProfileProperties
  where
  hashWithSalt
    _salt
    SalesforceConnectorProfileProperties' {..} =
      _salt
        `Prelude.hashWithSalt` instanceUrl
        `Prelude.hashWithSalt` isSandboxEnvironment

instance
  Prelude.NFData
    SalesforceConnectorProfileProperties
  where
  rnf SalesforceConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl `Prelude.seq`
      Prelude.rnf isSandboxEnvironment

instance
  Data.ToJSON
    SalesforceConnectorProfileProperties
  where
  toJSON SalesforceConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("instanceUrl" Data..=) Prelude.<$> instanceUrl,
            ("isSandboxEnvironment" Data..=)
              Prelude.<$> isSandboxEnvironment
          ]
      )
