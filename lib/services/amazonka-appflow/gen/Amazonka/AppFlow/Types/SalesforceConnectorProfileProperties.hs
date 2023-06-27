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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    isSandboxEnvironment :: Prelude.Maybe Prelude.Bool,
    -- | If the connection mode for the connector profile is private, this
    -- parameter sets whether Amazon AppFlow uses the private network to send
    -- metadata and authorization calls to Salesforce. Amazon AppFlow sends
    -- private calls through Amazon Web Services PrivateLink. These calls
    -- travel through Amazon Web Services infrastructure without being exposed
    -- to the public internet.
    --
    -- Set either of the following values:
    --
    -- [true]
    --     Amazon AppFlow sends all calls to Salesforce over the private
    --     network.
    --
    --     These private calls are:
    --
    --     -   Calls to get metadata about your Salesforce records. This
    --         metadata describes your Salesforce objects and their fields.
    --
    --     -   Calls to get or refresh access tokens that allow Amazon AppFlow
    --         to access your Salesforce records.
    --
    --     -   Calls to transfer your Salesforce records as part of a flow run.
    --
    -- [false]
    --     The default value. Amazon AppFlow sends some calls to Salesforce
    --     privately and other calls over the public internet.
    --
    --     The public calls are:
    --
    --     -   Calls to get metadata about your Salesforce records.
    --
    --     -   Calls to get or refresh access tokens.
    --
    --     The private calls are:
    --
    --     -   Calls to transfer your Salesforce records as part of a flow run.
    usePrivateLinkForMetadataAndAuthorization :: Prelude.Maybe Prelude.Bool
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
--
-- 'usePrivateLinkForMetadataAndAuthorization', 'salesforceConnectorProfileProperties_usePrivateLinkForMetadataAndAuthorization' - If the connection mode for the connector profile is private, this
-- parameter sets whether Amazon AppFlow uses the private network to send
-- metadata and authorization calls to Salesforce. Amazon AppFlow sends
-- private calls through Amazon Web Services PrivateLink. These calls
-- travel through Amazon Web Services infrastructure without being exposed
-- to the public internet.
--
-- Set either of the following values:
--
-- [true]
--     Amazon AppFlow sends all calls to Salesforce over the private
--     network.
--
--     These private calls are:
--
--     -   Calls to get metadata about your Salesforce records. This
--         metadata describes your Salesforce objects and their fields.
--
--     -   Calls to get or refresh access tokens that allow Amazon AppFlow
--         to access your Salesforce records.
--
--     -   Calls to transfer your Salesforce records as part of a flow run.
--
-- [false]
--     The default value. Amazon AppFlow sends some calls to Salesforce
--     privately and other calls over the public internet.
--
--     The public calls are:
--
--     -   Calls to get metadata about your Salesforce records.
--
--     -   Calls to get or refresh access tokens.
--
--     The private calls are:
--
--     -   Calls to transfer your Salesforce records as part of a flow run.
newSalesforceConnectorProfileProperties ::
  SalesforceConnectorProfileProperties
newSalesforceConnectorProfileProperties =
  SalesforceConnectorProfileProperties'
    { instanceUrl =
        Prelude.Nothing,
      isSandboxEnvironment =
        Prelude.Nothing,
      usePrivateLinkForMetadataAndAuthorization =
        Prelude.Nothing
    }

-- | The location of the Salesforce resource.
salesforceConnectorProfileProperties_instanceUrl :: Lens.Lens' SalesforceConnectorProfileProperties (Prelude.Maybe Prelude.Text)
salesforceConnectorProfileProperties_instanceUrl = Lens.lens (\SalesforceConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@SalesforceConnectorProfileProperties' {} a -> s {instanceUrl = a} :: SalesforceConnectorProfileProperties)

-- | Indicates whether the connector profile applies to a sandbox or
-- production environment.
salesforceConnectorProfileProperties_isSandboxEnvironment :: Lens.Lens' SalesforceConnectorProfileProperties (Prelude.Maybe Prelude.Bool)
salesforceConnectorProfileProperties_isSandboxEnvironment = Lens.lens (\SalesforceConnectorProfileProperties' {isSandboxEnvironment} -> isSandboxEnvironment) (\s@SalesforceConnectorProfileProperties' {} a -> s {isSandboxEnvironment = a} :: SalesforceConnectorProfileProperties)

-- | If the connection mode for the connector profile is private, this
-- parameter sets whether Amazon AppFlow uses the private network to send
-- metadata and authorization calls to Salesforce. Amazon AppFlow sends
-- private calls through Amazon Web Services PrivateLink. These calls
-- travel through Amazon Web Services infrastructure without being exposed
-- to the public internet.
--
-- Set either of the following values:
--
-- [true]
--     Amazon AppFlow sends all calls to Salesforce over the private
--     network.
--
--     These private calls are:
--
--     -   Calls to get metadata about your Salesforce records. This
--         metadata describes your Salesforce objects and their fields.
--
--     -   Calls to get or refresh access tokens that allow Amazon AppFlow
--         to access your Salesforce records.
--
--     -   Calls to transfer your Salesforce records as part of a flow run.
--
-- [false]
--     The default value. Amazon AppFlow sends some calls to Salesforce
--     privately and other calls over the public internet.
--
--     The public calls are:
--
--     -   Calls to get metadata about your Salesforce records.
--
--     -   Calls to get or refresh access tokens.
--
--     The private calls are:
--
--     -   Calls to transfer your Salesforce records as part of a flow run.
salesforceConnectorProfileProperties_usePrivateLinkForMetadataAndAuthorization :: Lens.Lens' SalesforceConnectorProfileProperties (Prelude.Maybe Prelude.Bool)
salesforceConnectorProfileProperties_usePrivateLinkForMetadataAndAuthorization = Lens.lens (\SalesforceConnectorProfileProperties' {usePrivateLinkForMetadataAndAuthorization} -> usePrivateLinkForMetadataAndAuthorization) (\s@SalesforceConnectorProfileProperties' {} a -> s {usePrivateLinkForMetadataAndAuthorization = a} :: SalesforceConnectorProfileProperties)

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
            Prelude.<*> ( x
                            Data..:? "usePrivateLinkForMetadataAndAuthorization"
                        )
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
        `Prelude.hashWithSalt` usePrivateLinkForMetadataAndAuthorization

instance
  Prelude.NFData
    SalesforceConnectorProfileProperties
  where
  rnf SalesforceConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl
      `Prelude.seq` Prelude.rnf isSandboxEnvironment
      `Prelude.seq` Prelude.rnf usePrivateLinkForMetadataAndAuthorization

instance
  Data.ToJSON
    SalesforceConnectorProfileProperties
  where
  toJSON SalesforceConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("instanceUrl" Data..=) Prelude.<$> instanceUrl,
            ("isSandboxEnvironment" Data..=)
              Prelude.<$> isSandboxEnvironment,
            ("usePrivateLinkForMetadataAndAuthorization" Data..=)
              Prelude.<$> usePrivateLinkForMetadataAndAuthorization
          ]
      )
