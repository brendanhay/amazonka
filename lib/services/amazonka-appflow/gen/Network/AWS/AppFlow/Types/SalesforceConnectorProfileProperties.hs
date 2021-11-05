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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  Core.FromJSON
    SalesforceConnectorProfileProperties
  where
  parseJSON =
    Core.withObject
      "SalesforceConnectorProfileProperties"
      ( \x ->
          SalesforceConnectorProfileProperties'
            Prelude.<$> (x Core..:? "instanceUrl")
            Prelude.<*> (x Core..:? "isSandboxEnvironment")
      )

instance
  Prelude.Hashable
    SalesforceConnectorProfileProperties

instance
  Prelude.NFData
    SalesforceConnectorProfileProperties

instance
  Core.ToJSON
    SalesforceConnectorProfileProperties
  where
  toJSON SalesforceConnectorProfileProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("instanceUrl" Core..=) Prelude.<$> instanceUrl,
            ("isSandboxEnvironment" Core..=)
              Prelude.<$> isSandboxEnvironment
          ]
      )
