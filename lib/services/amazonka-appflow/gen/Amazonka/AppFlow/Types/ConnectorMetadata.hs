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
-- Module      : Amazonka.AppFlow.Types.ConnectorMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorMetadata where

import Amazonka.AppFlow.Types.AmplitudeMetadata
import Amazonka.AppFlow.Types.CustomerProfilesMetadata
import Amazonka.AppFlow.Types.DatadogMetadata
import Amazonka.AppFlow.Types.DynatraceMetadata
import Amazonka.AppFlow.Types.EventBridgeMetadata
import Amazonka.AppFlow.Types.GoogleAnalyticsMetadata
import Amazonka.AppFlow.Types.HoneycodeMetadata
import Amazonka.AppFlow.Types.InforNexusMetadata
import Amazonka.AppFlow.Types.MarketoMetadata
import Amazonka.AppFlow.Types.RedshiftMetadata
import Amazonka.AppFlow.Types.S3Metadata
import Amazonka.AppFlow.Types.SAPODataMetadata
import Amazonka.AppFlow.Types.SalesforceMetadata
import Amazonka.AppFlow.Types.ServiceNowMetadata
import Amazonka.AppFlow.Types.SingularMetadata
import Amazonka.AppFlow.Types.SlackMetadata
import Amazonka.AppFlow.Types.SnowflakeMetadata
import Amazonka.AppFlow.Types.TrendmicroMetadata
import Amazonka.AppFlow.Types.UpsolverMetadata
import Amazonka.AppFlow.Types.VeevaMetadata
import Amazonka.AppFlow.Types.ZendeskMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure to specify connector-specific metadata such as
-- @oAuthScopes@, @supportedRegions@, @privateLinkServiceUrl@, and so on.
--
-- /See:/ 'newConnectorMetadata' smart constructor.
data ConnectorMetadata = ConnectorMetadata'
  { -- | The connector metadata specific to Zendesk.
    zendesk :: Prelude.Maybe ZendeskMetadata,
    -- | The connector metadata specific to Slack.
    slack :: Prelude.Maybe SlackMetadata,
    -- | The connector metadata specific to Singular.
    singular :: Prelude.Maybe SingularMetadata,
    -- | The connector metadata specific to Amazon S3.
    s3 :: Prelude.Maybe S3Metadata,
    -- | The connector metadata specific to Veeva.
    veeva :: Prelude.Maybe VeevaMetadata,
    -- | The connector metadata specific to Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeMetadata,
    -- | The connector metadata specific to Salesforce.
    salesforce :: Prelude.Maybe SalesforceMetadata,
    -- | The connector metadata specific to Snowflake.
    snowflake :: Prelude.Maybe SnowflakeMetadata,
    sAPOData :: Prelude.Maybe SAPODataMetadata,
    -- | The connector metadata specific to Marketo.
    marketo :: Prelude.Maybe MarketoMetadata,
    -- | The connector metadata specific to Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftMetadata,
    -- | The connector metadata specific to Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroMetadata,
    -- | The connector metadata specific to Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusMetadata,
    -- | The connector metadata specific to Amazon Connect Customer Profiles.
    customerProfiles :: Prelude.Maybe CustomerProfilesMetadata,
    -- | The connector metadata specific to Upsolver.
    upsolver :: Prelude.Maybe UpsolverMetadata,
    -- | The connector metadata specific to ServiceNow.
    serviceNow :: Prelude.Maybe ServiceNowMetadata,
    -- | The connector metadata specific to Datadog.
    datadog :: Prelude.Maybe DatadogMetadata,
    -- | The connector metadata specific to Amplitude.
    amplitude :: Prelude.Maybe AmplitudeMetadata,
    -- | The connector metadata specific to Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceMetadata,
    -- | The connector metadata specific to Google Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsMetadata,
    -- | The connector metadata specific to Amazon EventBridge.
    eventBridge :: Prelude.Maybe EventBridgeMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zendesk', 'connectorMetadata_zendesk' - The connector metadata specific to Zendesk.
--
-- 'slack', 'connectorMetadata_slack' - The connector metadata specific to Slack.
--
-- 'singular', 'connectorMetadata_singular' - The connector metadata specific to Singular.
--
-- 's3', 'connectorMetadata_s3' - The connector metadata specific to Amazon S3.
--
-- 'veeva', 'connectorMetadata_veeva' - The connector metadata specific to Veeva.
--
-- 'honeycode', 'connectorMetadata_honeycode' - The connector metadata specific to Amazon Honeycode.
--
-- 'salesforce', 'connectorMetadata_salesforce' - The connector metadata specific to Salesforce.
--
-- 'snowflake', 'connectorMetadata_snowflake' - The connector metadata specific to Snowflake.
--
-- 'sAPOData', 'connectorMetadata_sAPOData' - Undocumented member.
--
-- 'marketo', 'connectorMetadata_marketo' - The connector metadata specific to Marketo.
--
-- 'redshift', 'connectorMetadata_redshift' - The connector metadata specific to Amazon Redshift.
--
-- 'trendmicro', 'connectorMetadata_trendmicro' - The connector metadata specific to Trend Micro.
--
-- 'inforNexus', 'connectorMetadata_inforNexus' - The connector metadata specific to Infor Nexus.
--
-- 'customerProfiles', 'connectorMetadata_customerProfiles' - The connector metadata specific to Amazon Connect Customer Profiles.
--
-- 'upsolver', 'connectorMetadata_upsolver' - The connector metadata specific to Upsolver.
--
-- 'serviceNow', 'connectorMetadata_serviceNow' - The connector metadata specific to ServiceNow.
--
-- 'datadog', 'connectorMetadata_datadog' - The connector metadata specific to Datadog.
--
-- 'amplitude', 'connectorMetadata_amplitude' - The connector metadata specific to Amplitude.
--
-- 'dynatrace', 'connectorMetadata_dynatrace' - The connector metadata specific to Dynatrace.
--
-- 'googleAnalytics', 'connectorMetadata_googleAnalytics' - The connector metadata specific to Google Analytics.
--
-- 'eventBridge', 'connectorMetadata_eventBridge' - The connector metadata specific to Amazon EventBridge.
newConnectorMetadata ::
  ConnectorMetadata
newConnectorMetadata =
  ConnectorMetadata'
    { zendesk = Prelude.Nothing,
      slack = Prelude.Nothing,
      singular = Prelude.Nothing,
      s3 = Prelude.Nothing,
      veeva = Prelude.Nothing,
      honeycode = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      snowflake = Prelude.Nothing,
      sAPOData = Prelude.Nothing,
      marketo = Prelude.Nothing,
      redshift = Prelude.Nothing,
      trendmicro = Prelude.Nothing,
      inforNexus = Prelude.Nothing,
      customerProfiles = Prelude.Nothing,
      upsolver = Prelude.Nothing,
      serviceNow = Prelude.Nothing,
      datadog = Prelude.Nothing,
      amplitude = Prelude.Nothing,
      dynatrace = Prelude.Nothing,
      googleAnalytics = Prelude.Nothing,
      eventBridge = Prelude.Nothing
    }

-- | The connector metadata specific to Zendesk.
connectorMetadata_zendesk :: Lens.Lens' ConnectorMetadata (Prelude.Maybe ZendeskMetadata)
connectorMetadata_zendesk = Lens.lens (\ConnectorMetadata' {zendesk} -> zendesk) (\s@ConnectorMetadata' {} a -> s {zendesk = a} :: ConnectorMetadata)

-- | The connector metadata specific to Slack.
connectorMetadata_slack :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SlackMetadata)
connectorMetadata_slack = Lens.lens (\ConnectorMetadata' {slack} -> slack) (\s@ConnectorMetadata' {} a -> s {slack = a} :: ConnectorMetadata)

-- | The connector metadata specific to Singular.
connectorMetadata_singular :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SingularMetadata)
connectorMetadata_singular = Lens.lens (\ConnectorMetadata' {singular} -> singular) (\s@ConnectorMetadata' {} a -> s {singular = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon S3.
connectorMetadata_s3 :: Lens.Lens' ConnectorMetadata (Prelude.Maybe S3Metadata)
connectorMetadata_s3 = Lens.lens (\ConnectorMetadata' {s3} -> s3) (\s@ConnectorMetadata' {} a -> s {s3 = a} :: ConnectorMetadata)

-- | The connector metadata specific to Veeva.
connectorMetadata_veeva :: Lens.Lens' ConnectorMetadata (Prelude.Maybe VeevaMetadata)
connectorMetadata_veeva = Lens.lens (\ConnectorMetadata' {veeva} -> veeva) (\s@ConnectorMetadata' {} a -> s {veeva = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon Honeycode.
connectorMetadata_honeycode :: Lens.Lens' ConnectorMetadata (Prelude.Maybe HoneycodeMetadata)
connectorMetadata_honeycode = Lens.lens (\ConnectorMetadata' {honeycode} -> honeycode) (\s@ConnectorMetadata' {} a -> s {honeycode = a} :: ConnectorMetadata)

-- | The connector metadata specific to Salesforce.
connectorMetadata_salesforce :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SalesforceMetadata)
connectorMetadata_salesforce = Lens.lens (\ConnectorMetadata' {salesforce} -> salesforce) (\s@ConnectorMetadata' {} a -> s {salesforce = a} :: ConnectorMetadata)

-- | The connector metadata specific to Snowflake.
connectorMetadata_snowflake :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SnowflakeMetadata)
connectorMetadata_snowflake = Lens.lens (\ConnectorMetadata' {snowflake} -> snowflake) (\s@ConnectorMetadata' {} a -> s {snowflake = a} :: ConnectorMetadata)

-- | Undocumented member.
connectorMetadata_sAPOData :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SAPODataMetadata)
connectorMetadata_sAPOData = Lens.lens (\ConnectorMetadata' {sAPOData} -> sAPOData) (\s@ConnectorMetadata' {} a -> s {sAPOData = a} :: ConnectorMetadata)

-- | The connector metadata specific to Marketo.
connectorMetadata_marketo :: Lens.Lens' ConnectorMetadata (Prelude.Maybe MarketoMetadata)
connectorMetadata_marketo = Lens.lens (\ConnectorMetadata' {marketo} -> marketo) (\s@ConnectorMetadata' {} a -> s {marketo = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon Redshift.
connectorMetadata_redshift :: Lens.Lens' ConnectorMetadata (Prelude.Maybe RedshiftMetadata)
connectorMetadata_redshift = Lens.lens (\ConnectorMetadata' {redshift} -> redshift) (\s@ConnectorMetadata' {} a -> s {redshift = a} :: ConnectorMetadata)

-- | The connector metadata specific to Trend Micro.
connectorMetadata_trendmicro :: Lens.Lens' ConnectorMetadata (Prelude.Maybe TrendmicroMetadata)
connectorMetadata_trendmicro = Lens.lens (\ConnectorMetadata' {trendmicro} -> trendmicro) (\s@ConnectorMetadata' {} a -> s {trendmicro = a} :: ConnectorMetadata)

-- | The connector metadata specific to Infor Nexus.
connectorMetadata_inforNexus :: Lens.Lens' ConnectorMetadata (Prelude.Maybe InforNexusMetadata)
connectorMetadata_inforNexus = Lens.lens (\ConnectorMetadata' {inforNexus} -> inforNexus) (\s@ConnectorMetadata' {} a -> s {inforNexus = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon Connect Customer Profiles.
connectorMetadata_customerProfiles :: Lens.Lens' ConnectorMetadata (Prelude.Maybe CustomerProfilesMetadata)
connectorMetadata_customerProfiles = Lens.lens (\ConnectorMetadata' {customerProfiles} -> customerProfiles) (\s@ConnectorMetadata' {} a -> s {customerProfiles = a} :: ConnectorMetadata)

-- | The connector metadata specific to Upsolver.
connectorMetadata_upsolver :: Lens.Lens' ConnectorMetadata (Prelude.Maybe UpsolverMetadata)
connectorMetadata_upsolver = Lens.lens (\ConnectorMetadata' {upsolver} -> upsolver) (\s@ConnectorMetadata' {} a -> s {upsolver = a} :: ConnectorMetadata)

-- | The connector metadata specific to ServiceNow.
connectorMetadata_serviceNow :: Lens.Lens' ConnectorMetadata (Prelude.Maybe ServiceNowMetadata)
connectorMetadata_serviceNow = Lens.lens (\ConnectorMetadata' {serviceNow} -> serviceNow) (\s@ConnectorMetadata' {} a -> s {serviceNow = a} :: ConnectorMetadata)

-- | The connector metadata specific to Datadog.
connectorMetadata_datadog :: Lens.Lens' ConnectorMetadata (Prelude.Maybe DatadogMetadata)
connectorMetadata_datadog = Lens.lens (\ConnectorMetadata' {datadog} -> datadog) (\s@ConnectorMetadata' {} a -> s {datadog = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amplitude.
connectorMetadata_amplitude :: Lens.Lens' ConnectorMetadata (Prelude.Maybe AmplitudeMetadata)
connectorMetadata_amplitude = Lens.lens (\ConnectorMetadata' {amplitude} -> amplitude) (\s@ConnectorMetadata' {} a -> s {amplitude = a} :: ConnectorMetadata)

-- | The connector metadata specific to Dynatrace.
connectorMetadata_dynatrace :: Lens.Lens' ConnectorMetadata (Prelude.Maybe DynatraceMetadata)
connectorMetadata_dynatrace = Lens.lens (\ConnectorMetadata' {dynatrace} -> dynatrace) (\s@ConnectorMetadata' {} a -> s {dynatrace = a} :: ConnectorMetadata)

-- | The connector metadata specific to Google Analytics.
connectorMetadata_googleAnalytics :: Lens.Lens' ConnectorMetadata (Prelude.Maybe GoogleAnalyticsMetadata)
connectorMetadata_googleAnalytics = Lens.lens (\ConnectorMetadata' {googleAnalytics} -> googleAnalytics) (\s@ConnectorMetadata' {} a -> s {googleAnalytics = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon EventBridge.
connectorMetadata_eventBridge :: Lens.Lens' ConnectorMetadata (Prelude.Maybe EventBridgeMetadata)
connectorMetadata_eventBridge = Lens.lens (\ConnectorMetadata' {eventBridge} -> eventBridge) (\s@ConnectorMetadata' {} a -> s {eventBridge = a} :: ConnectorMetadata)

instance Data.FromJSON ConnectorMetadata where
  parseJSON =
    Data.withObject
      "ConnectorMetadata"
      ( \x ->
          ConnectorMetadata'
            Prelude.<$> (x Data..:? "Zendesk")
            Prelude.<*> (x Data..:? "Slack")
            Prelude.<*> (x Data..:? "Singular")
            Prelude.<*> (x Data..:? "S3")
            Prelude.<*> (x Data..:? "Veeva")
            Prelude.<*> (x Data..:? "Honeycode")
            Prelude.<*> (x Data..:? "Salesforce")
            Prelude.<*> (x Data..:? "Snowflake")
            Prelude.<*> (x Data..:? "SAPOData")
            Prelude.<*> (x Data..:? "Marketo")
            Prelude.<*> (x Data..:? "Redshift")
            Prelude.<*> (x Data..:? "Trendmicro")
            Prelude.<*> (x Data..:? "InforNexus")
            Prelude.<*> (x Data..:? "CustomerProfiles")
            Prelude.<*> (x Data..:? "Upsolver")
            Prelude.<*> (x Data..:? "ServiceNow")
            Prelude.<*> (x Data..:? "Datadog")
            Prelude.<*> (x Data..:? "Amplitude")
            Prelude.<*> (x Data..:? "Dynatrace")
            Prelude.<*> (x Data..:? "GoogleAnalytics")
            Prelude.<*> (x Data..:? "EventBridge")
      )

instance Prelude.Hashable ConnectorMetadata where
  hashWithSalt _salt ConnectorMetadata' {..} =
    _salt `Prelude.hashWithSalt` zendesk
      `Prelude.hashWithSalt` slack
      `Prelude.hashWithSalt` singular
      `Prelude.hashWithSalt` s3
      `Prelude.hashWithSalt` veeva
      `Prelude.hashWithSalt` honeycode
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` snowflake
      `Prelude.hashWithSalt` sAPOData
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` redshift
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` inforNexus
      `Prelude.hashWithSalt` customerProfiles
      `Prelude.hashWithSalt` upsolver
      `Prelude.hashWithSalt` serviceNow
      `Prelude.hashWithSalt` datadog
      `Prelude.hashWithSalt` amplitude
      `Prelude.hashWithSalt` dynatrace
      `Prelude.hashWithSalt` googleAnalytics
      `Prelude.hashWithSalt` eventBridge

instance Prelude.NFData ConnectorMetadata where
  rnf ConnectorMetadata' {..} =
    Prelude.rnf zendesk
      `Prelude.seq` Prelude.rnf slack
      `Prelude.seq` Prelude.rnf singular
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf veeva
      `Prelude.seq` Prelude.rnf honeycode
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf snowflake
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf redshift
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf inforNexus
      `Prelude.seq` Prelude.rnf customerProfiles
      `Prelude.seq` Prelude.rnf upsolver
      `Prelude.seq` Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf datadog
      `Prelude.seq` Prelude.rnf amplitude
      `Prelude.seq` Prelude.rnf dynatrace
      `Prelude.seq` Prelude.rnf googleAnalytics
      `Prelude.seq` Prelude.rnf eventBridge
