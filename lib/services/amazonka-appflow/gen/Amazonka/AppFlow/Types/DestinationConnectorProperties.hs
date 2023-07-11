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
-- Module      : Amazonka.AppFlow.Types.DestinationConnectorProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DestinationConnectorProperties where

import Amazonka.AppFlow.Types.CustomConnectorDestinationProperties
import Amazonka.AppFlow.Types.CustomerProfilesDestinationProperties
import Amazonka.AppFlow.Types.EventBridgeDestinationProperties
import Amazonka.AppFlow.Types.HoneycodeDestinationProperties
import Amazonka.AppFlow.Types.LookoutMetricsDestinationProperties
import Amazonka.AppFlow.Types.MarketoDestinationProperties
import Amazonka.AppFlow.Types.RedshiftDestinationProperties
import Amazonka.AppFlow.Types.S3DestinationProperties
import Amazonka.AppFlow.Types.SAPODataDestinationProperties
import Amazonka.AppFlow.Types.SalesforceDestinationProperties
import Amazonka.AppFlow.Types.SnowflakeDestinationProperties
import Amazonka.AppFlow.Types.UpsolverDestinationProperties
import Amazonka.AppFlow.Types.ZendeskDestinationProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This stores the information that is required to query a particular
-- connector.
--
-- /See:/ 'newDestinationConnectorProperties' smart constructor.
data DestinationConnectorProperties = DestinationConnectorProperties'
  { -- | The properties that are required to query the custom Connector.
    customConnector :: Prelude.Maybe CustomConnectorDestinationProperties,
    -- | The properties required to query Amazon Connect Customer Profiles.
    customerProfiles :: Prelude.Maybe CustomerProfilesDestinationProperties,
    -- | The properties required to query Amazon EventBridge.
    eventBridge :: Prelude.Maybe EventBridgeDestinationProperties,
    -- | The properties required to query Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeDestinationProperties,
    -- | The properties required to query Amazon Lookout for Metrics.
    lookoutMetrics :: Prelude.Maybe LookoutMetricsDestinationProperties,
    -- | The properties required to query Marketo.
    marketo :: Prelude.Maybe MarketoDestinationProperties,
    -- | The properties required to query Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftDestinationProperties,
    -- | The properties required to query Amazon S3.
    s3 :: Prelude.Maybe S3DestinationProperties,
    -- | The properties required to query SAPOData.
    sAPOData :: Prelude.Maybe SAPODataDestinationProperties,
    -- | The properties required to query Salesforce.
    salesforce :: Prelude.Maybe SalesforceDestinationProperties,
    -- | The properties required to query Snowflake.
    snowflake :: Prelude.Maybe SnowflakeDestinationProperties,
    -- | The properties required to query Upsolver.
    upsolver :: Prelude.Maybe UpsolverDestinationProperties,
    -- | The properties required to query Zendesk.
    zendesk :: Prelude.Maybe ZendeskDestinationProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationConnectorProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customConnector', 'destinationConnectorProperties_customConnector' - The properties that are required to query the custom Connector.
--
-- 'customerProfiles', 'destinationConnectorProperties_customerProfiles' - The properties required to query Amazon Connect Customer Profiles.
--
-- 'eventBridge', 'destinationConnectorProperties_eventBridge' - The properties required to query Amazon EventBridge.
--
-- 'honeycode', 'destinationConnectorProperties_honeycode' - The properties required to query Amazon Honeycode.
--
-- 'lookoutMetrics', 'destinationConnectorProperties_lookoutMetrics' - The properties required to query Amazon Lookout for Metrics.
--
-- 'marketo', 'destinationConnectorProperties_marketo' - The properties required to query Marketo.
--
-- 'redshift', 'destinationConnectorProperties_redshift' - The properties required to query Amazon Redshift.
--
-- 's3', 'destinationConnectorProperties_s3' - The properties required to query Amazon S3.
--
-- 'sAPOData', 'destinationConnectorProperties_sAPOData' - The properties required to query SAPOData.
--
-- 'salesforce', 'destinationConnectorProperties_salesforce' - The properties required to query Salesforce.
--
-- 'snowflake', 'destinationConnectorProperties_snowflake' - The properties required to query Snowflake.
--
-- 'upsolver', 'destinationConnectorProperties_upsolver' - The properties required to query Upsolver.
--
-- 'zendesk', 'destinationConnectorProperties_zendesk' - The properties required to query Zendesk.
newDestinationConnectorProperties ::
  DestinationConnectorProperties
newDestinationConnectorProperties =
  DestinationConnectorProperties'
    { customConnector =
        Prelude.Nothing,
      customerProfiles = Prelude.Nothing,
      eventBridge = Prelude.Nothing,
      honeycode = Prelude.Nothing,
      lookoutMetrics = Prelude.Nothing,
      marketo = Prelude.Nothing,
      redshift = Prelude.Nothing,
      s3 = Prelude.Nothing,
      sAPOData = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      snowflake = Prelude.Nothing,
      upsolver = Prelude.Nothing,
      zendesk = Prelude.Nothing
    }

-- | The properties that are required to query the custom Connector.
destinationConnectorProperties_customConnector :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe CustomConnectorDestinationProperties)
destinationConnectorProperties_customConnector = Lens.lens (\DestinationConnectorProperties' {customConnector} -> customConnector) (\s@DestinationConnectorProperties' {} a -> s {customConnector = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon Connect Customer Profiles.
destinationConnectorProperties_customerProfiles :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe CustomerProfilesDestinationProperties)
destinationConnectorProperties_customerProfiles = Lens.lens (\DestinationConnectorProperties' {customerProfiles} -> customerProfiles) (\s@DestinationConnectorProperties' {} a -> s {customerProfiles = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon EventBridge.
destinationConnectorProperties_eventBridge :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe EventBridgeDestinationProperties)
destinationConnectorProperties_eventBridge = Lens.lens (\DestinationConnectorProperties' {eventBridge} -> eventBridge) (\s@DestinationConnectorProperties' {} a -> s {eventBridge = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon Honeycode.
destinationConnectorProperties_honeycode :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe HoneycodeDestinationProperties)
destinationConnectorProperties_honeycode = Lens.lens (\DestinationConnectorProperties' {honeycode} -> honeycode) (\s@DestinationConnectorProperties' {} a -> s {honeycode = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon Lookout for Metrics.
destinationConnectorProperties_lookoutMetrics :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe LookoutMetricsDestinationProperties)
destinationConnectorProperties_lookoutMetrics = Lens.lens (\DestinationConnectorProperties' {lookoutMetrics} -> lookoutMetrics) (\s@DestinationConnectorProperties' {} a -> s {lookoutMetrics = a} :: DestinationConnectorProperties)

-- | The properties required to query Marketo.
destinationConnectorProperties_marketo :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe MarketoDestinationProperties)
destinationConnectorProperties_marketo = Lens.lens (\DestinationConnectorProperties' {marketo} -> marketo) (\s@DestinationConnectorProperties' {} a -> s {marketo = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon Redshift.
destinationConnectorProperties_redshift :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe RedshiftDestinationProperties)
destinationConnectorProperties_redshift = Lens.lens (\DestinationConnectorProperties' {redshift} -> redshift) (\s@DestinationConnectorProperties' {} a -> s {redshift = a} :: DestinationConnectorProperties)

-- | The properties required to query Amazon S3.
destinationConnectorProperties_s3 :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe S3DestinationProperties)
destinationConnectorProperties_s3 = Lens.lens (\DestinationConnectorProperties' {s3} -> s3) (\s@DestinationConnectorProperties' {} a -> s {s3 = a} :: DestinationConnectorProperties)

-- | The properties required to query SAPOData.
destinationConnectorProperties_sAPOData :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe SAPODataDestinationProperties)
destinationConnectorProperties_sAPOData = Lens.lens (\DestinationConnectorProperties' {sAPOData} -> sAPOData) (\s@DestinationConnectorProperties' {} a -> s {sAPOData = a} :: DestinationConnectorProperties)

-- | The properties required to query Salesforce.
destinationConnectorProperties_salesforce :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe SalesforceDestinationProperties)
destinationConnectorProperties_salesforce = Lens.lens (\DestinationConnectorProperties' {salesforce} -> salesforce) (\s@DestinationConnectorProperties' {} a -> s {salesforce = a} :: DestinationConnectorProperties)

-- | The properties required to query Snowflake.
destinationConnectorProperties_snowflake :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe SnowflakeDestinationProperties)
destinationConnectorProperties_snowflake = Lens.lens (\DestinationConnectorProperties' {snowflake} -> snowflake) (\s@DestinationConnectorProperties' {} a -> s {snowflake = a} :: DestinationConnectorProperties)

-- | The properties required to query Upsolver.
destinationConnectorProperties_upsolver :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe UpsolverDestinationProperties)
destinationConnectorProperties_upsolver = Lens.lens (\DestinationConnectorProperties' {upsolver} -> upsolver) (\s@DestinationConnectorProperties' {} a -> s {upsolver = a} :: DestinationConnectorProperties)

-- | The properties required to query Zendesk.
destinationConnectorProperties_zendesk :: Lens.Lens' DestinationConnectorProperties (Prelude.Maybe ZendeskDestinationProperties)
destinationConnectorProperties_zendesk = Lens.lens (\DestinationConnectorProperties' {zendesk} -> zendesk) (\s@DestinationConnectorProperties' {} a -> s {zendesk = a} :: DestinationConnectorProperties)

instance Data.FromJSON DestinationConnectorProperties where
  parseJSON =
    Data.withObject
      "DestinationConnectorProperties"
      ( \x ->
          DestinationConnectorProperties'
            Prelude.<$> (x Data..:? "CustomConnector")
            Prelude.<*> (x Data..:? "CustomerProfiles")
            Prelude.<*> (x Data..:? "EventBridge")
            Prelude.<*> (x Data..:? "Honeycode")
            Prelude.<*> (x Data..:? "LookoutMetrics")
            Prelude.<*> (x Data..:? "Marketo")
            Prelude.<*> (x Data..:? "Redshift")
            Prelude.<*> (x Data..:? "S3")
            Prelude.<*> (x Data..:? "SAPOData")
            Prelude.<*> (x Data..:? "Salesforce")
            Prelude.<*> (x Data..:? "Snowflake")
            Prelude.<*> (x Data..:? "Upsolver")
            Prelude.<*> (x Data..:? "Zendesk")
      )

instance
  Prelude.Hashable
    DestinationConnectorProperties
  where
  hashWithSalt
    _salt
    DestinationConnectorProperties' {..} =
      _salt
        `Prelude.hashWithSalt` customConnector
        `Prelude.hashWithSalt` customerProfiles
        `Prelude.hashWithSalt` eventBridge
        `Prelude.hashWithSalt` honeycode
        `Prelude.hashWithSalt` lookoutMetrics
        `Prelude.hashWithSalt` marketo
        `Prelude.hashWithSalt` redshift
        `Prelude.hashWithSalt` s3
        `Prelude.hashWithSalt` sAPOData
        `Prelude.hashWithSalt` salesforce
        `Prelude.hashWithSalt` snowflake
        `Prelude.hashWithSalt` upsolver
        `Prelude.hashWithSalt` zendesk

instance
  Prelude.NFData
    DestinationConnectorProperties
  where
  rnf DestinationConnectorProperties' {..} =
    Prelude.rnf customConnector
      `Prelude.seq` Prelude.rnf customerProfiles
      `Prelude.seq` Prelude.rnf eventBridge
      `Prelude.seq` Prelude.rnf honeycode
      `Prelude.seq` Prelude.rnf lookoutMetrics
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf redshift
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf snowflake
      `Prelude.seq` Prelude.rnf upsolver
      `Prelude.seq` Prelude.rnf zendesk

instance Data.ToJSON DestinationConnectorProperties where
  toJSON DestinationConnectorProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomConnector" Data..=)
              Prelude.<$> customConnector,
            ("CustomerProfiles" Data..=)
              Prelude.<$> customerProfiles,
            ("EventBridge" Data..=) Prelude.<$> eventBridge,
            ("Honeycode" Data..=) Prelude.<$> honeycode,
            ("LookoutMetrics" Data..=)
              Prelude.<$> lookoutMetrics,
            ("Marketo" Data..=) Prelude.<$> marketo,
            ("Redshift" Data..=) Prelude.<$> redshift,
            ("S3" Data..=) Prelude.<$> s3,
            ("SAPOData" Data..=) Prelude.<$> sAPOData,
            ("Salesforce" Data..=) Prelude.<$> salesforce,
            ("Snowflake" Data..=) Prelude.<$> snowflake,
            ("Upsolver" Data..=) Prelude.<$> upsolver,
            ("Zendesk" Data..=) Prelude.<$> zendesk
          ]
      )
