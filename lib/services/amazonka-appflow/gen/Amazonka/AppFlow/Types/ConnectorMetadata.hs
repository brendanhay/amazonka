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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure to specify connector-specific metadata such as
-- @oAuthScopes@, @supportedRegions@, @privateLinkServiceUrl@, and so on.
--
-- /See:/ 'newConnectorMetadata' smart constructor.
data ConnectorMetadata = ConnectorMetadata'
  { -- | The connector metadata specific to Upsolver.
    upsolver :: Prelude.Maybe UpsolverMetadata,
    -- | The connector metadata specific to Snowflake.
    snowflake :: Prelude.Maybe SnowflakeMetadata,
    -- | The connector metadata specific to Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeMetadata,
    -- | The connector metadata specific to ServiceNow.
    serviceNow :: Prelude.Maybe ServiceNowMetadata,
    -- | The connector metadata specific to Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceMetadata,
    -- | The connector metadata specific to Marketo.
    marketo :: Prelude.Maybe MarketoMetadata,
    -- | The connector metadata specific to Slack.
    slack :: Prelude.Maybe SlackMetadata,
    -- | The connector metadata specific to Singular.
    singular :: Prelude.Maybe SingularMetadata,
    -- | The connector metadata specific to Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusMetadata,
    -- | The connector metadata specific to Amplitude.
    amplitude :: Prelude.Maybe AmplitudeMetadata,
    -- | The connector metadata specific to Amazon Connect Customer Profiles.
    customerProfiles :: Prelude.Maybe CustomerProfilesMetadata,
    -- | The connector metadata specific to Datadog.
    datadog :: Prelude.Maybe DatadogMetadata,
    -- | The connector metadata specific to Google Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsMetadata,
    sAPOData :: Prelude.Maybe SAPODataMetadata,
    -- | The connector metadata specific to Salesforce.
    salesforce :: Prelude.Maybe SalesforceMetadata,
    -- | The connector metadata specific to Zendesk.
    zendesk :: Prelude.Maybe ZendeskMetadata,
    -- | The connector metadata specific to Amazon S3.
    s3 :: Prelude.Maybe S3Metadata,
    -- | The connector metadata specific to Amazon EventBridge.
    eventBridge :: Prelude.Maybe EventBridgeMetadata,
    -- | The connector metadata specific to Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroMetadata,
    -- | The connector metadata specific to Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftMetadata,
    -- | The connector metadata specific to Veeva.
    veeva :: Prelude.Maybe VeevaMetadata
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
-- 'upsolver', 'connectorMetadata_upsolver' - The connector metadata specific to Upsolver.
--
-- 'snowflake', 'connectorMetadata_snowflake' - The connector metadata specific to Snowflake.
--
-- 'honeycode', 'connectorMetadata_honeycode' - The connector metadata specific to Amazon Honeycode.
--
-- 'serviceNow', 'connectorMetadata_serviceNow' - The connector metadata specific to ServiceNow.
--
-- 'dynatrace', 'connectorMetadata_dynatrace' - The connector metadata specific to Dynatrace.
--
-- 'marketo', 'connectorMetadata_marketo' - The connector metadata specific to Marketo.
--
-- 'slack', 'connectorMetadata_slack' - The connector metadata specific to Slack.
--
-- 'singular', 'connectorMetadata_singular' - The connector metadata specific to Singular.
--
-- 'inforNexus', 'connectorMetadata_inforNexus' - The connector metadata specific to Infor Nexus.
--
-- 'amplitude', 'connectorMetadata_amplitude' - The connector metadata specific to Amplitude.
--
-- 'customerProfiles', 'connectorMetadata_customerProfiles' - The connector metadata specific to Amazon Connect Customer Profiles.
--
-- 'datadog', 'connectorMetadata_datadog' - The connector metadata specific to Datadog.
--
-- 'googleAnalytics', 'connectorMetadata_googleAnalytics' - The connector metadata specific to Google Analytics.
--
-- 'sAPOData', 'connectorMetadata_sAPOData' - Undocumented member.
--
-- 'salesforce', 'connectorMetadata_salesforce' - The connector metadata specific to Salesforce.
--
-- 'zendesk', 'connectorMetadata_zendesk' - The connector metadata specific to Zendesk.
--
-- 's3', 'connectorMetadata_s3' - The connector metadata specific to Amazon S3.
--
-- 'eventBridge', 'connectorMetadata_eventBridge' - The connector metadata specific to Amazon EventBridge.
--
-- 'trendmicro', 'connectorMetadata_trendmicro' - The connector metadata specific to Trend Micro.
--
-- 'redshift', 'connectorMetadata_redshift' - The connector metadata specific to Amazon Redshift.
--
-- 'veeva', 'connectorMetadata_veeva' - The connector metadata specific to Veeva.
newConnectorMetadata ::
  ConnectorMetadata
newConnectorMetadata =
  ConnectorMetadata'
    { upsolver = Prelude.Nothing,
      snowflake = Prelude.Nothing,
      honeycode = Prelude.Nothing,
      serviceNow = Prelude.Nothing,
      dynatrace = Prelude.Nothing,
      marketo = Prelude.Nothing,
      slack = Prelude.Nothing,
      singular = Prelude.Nothing,
      inforNexus = Prelude.Nothing,
      amplitude = Prelude.Nothing,
      customerProfiles = Prelude.Nothing,
      datadog = Prelude.Nothing,
      googleAnalytics = Prelude.Nothing,
      sAPOData = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      zendesk = Prelude.Nothing,
      s3 = Prelude.Nothing,
      eventBridge = Prelude.Nothing,
      trendmicro = Prelude.Nothing,
      redshift = Prelude.Nothing,
      veeva = Prelude.Nothing
    }

-- | The connector metadata specific to Upsolver.
connectorMetadata_upsolver :: Lens.Lens' ConnectorMetadata (Prelude.Maybe UpsolverMetadata)
connectorMetadata_upsolver = Lens.lens (\ConnectorMetadata' {upsolver} -> upsolver) (\s@ConnectorMetadata' {} a -> s {upsolver = a} :: ConnectorMetadata)

-- | The connector metadata specific to Snowflake.
connectorMetadata_snowflake :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SnowflakeMetadata)
connectorMetadata_snowflake = Lens.lens (\ConnectorMetadata' {snowflake} -> snowflake) (\s@ConnectorMetadata' {} a -> s {snowflake = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon Honeycode.
connectorMetadata_honeycode :: Lens.Lens' ConnectorMetadata (Prelude.Maybe HoneycodeMetadata)
connectorMetadata_honeycode = Lens.lens (\ConnectorMetadata' {honeycode} -> honeycode) (\s@ConnectorMetadata' {} a -> s {honeycode = a} :: ConnectorMetadata)

-- | The connector metadata specific to ServiceNow.
connectorMetadata_serviceNow :: Lens.Lens' ConnectorMetadata (Prelude.Maybe ServiceNowMetadata)
connectorMetadata_serviceNow = Lens.lens (\ConnectorMetadata' {serviceNow} -> serviceNow) (\s@ConnectorMetadata' {} a -> s {serviceNow = a} :: ConnectorMetadata)

-- | The connector metadata specific to Dynatrace.
connectorMetadata_dynatrace :: Lens.Lens' ConnectorMetadata (Prelude.Maybe DynatraceMetadata)
connectorMetadata_dynatrace = Lens.lens (\ConnectorMetadata' {dynatrace} -> dynatrace) (\s@ConnectorMetadata' {} a -> s {dynatrace = a} :: ConnectorMetadata)

-- | The connector metadata specific to Marketo.
connectorMetadata_marketo :: Lens.Lens' ConnectorMetadata (Prelude.Maybe MarketoMetadata)
connectorMetadata_marketo = Lens.lens (\ConnectorMetadata' {marketo} -> marketo) (\s@ConnectorMetadata' {} a -> s {marketo = a} :: ConnectorMetadata)

-- | The connector metadata specific to Slack.
connectorMetadata_slack :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SlackMetadata)
connectorMetadata_slack = Lens.lens (\ConnectorMetadata' {slack} -> slack) (\s@ConnectorMetadata' {} a -> s {slack = a} :: ConnectorMetadata)

-- | The connector metadata specific to Singular.
connectorMetadata_singular :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SingularMetadata)
connectorMetadata_singular = Lens.lens (\ConnectorMetadata' {singular} -> singular) (\s@ConnectorMetadata' {} a -> s {singular = a} :: ConnectorMetadata)

-- | The connector metadata specific to Infor Nexus.
connectorMetadata_inforNexus :: Lens.Lens' ConnectorMetadata (Prelude.Maybe InforNexusMetadata)
connectorMetadata_inforNexus = Lens.lens (\ConnectorMetadata' {inforNexus} -> inforNexus) (\s@ConnectorMetadata' {} a -> s {inforNexus = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amplitude.
connectorMetadata_amplitude :: Lens.Lens' ConnectorMetadata (Prelude.Maybe AmplitudeMetadata)
connectorMetadata_amplitude = Lens.lens (\ConnectorMetadata' {amplitude} -> amplitude) (\s@ConnectorMetadata' {} a -> s {amplitude = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon Connect Customer Profiles.
connectorMetadata_customerProfiles :: Lens.Lens' ConnectorMetadata (Prelude.Maybe CustomerProfilesMetadata)
connectorMetadata_customerProfiles = Lens.lens (\ConnectorMetadata' {customerProfiles} -> customerProfiles) (\s@ConnectorMetadata' {} a -> s {customerProfiles = a} :: ConnectorMetadata)

-- | The connector metadata specific to Datadog.
connectorMetadata_datadog :: Lens.Lens' ConnectorMetadata (Prelude.Maybe DatadogMetadata)
connectorMetadata_datadog = Lens.lens (\ConnectorMetadata' {datadog} -> datadog) (\s@ConnectorMetadata' {} a -> s {datadog = a} :: ConnectorMetadata)

-- | The connector metadata specific to Google Analytics.
connectorMetadata_googleAnalytics :: Lens.Lens' ConnectorMetadata (Prelude.Maybe GoogleAnalyticsMetadata)
connectorMetadata_googleAnalytics = Lens.lens (\ConnectorMetadata' {googleAnalytics} -> googleAnalytics) (\s@ConnectorMetadata' {} a -> s {googleAnalytics = a} :: ConnectorMetadata)

-- | Undocumented member.
connectorMetadata_sAPOData :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SAPODataMetadata)
connectorMetadata_sAPOData = Lens.lens (\ConnectorMetadata' {sAPOData} -> sAPOData) (\s@ConnectorMetadata' {} a -> s {sAPOData = a} :: ConnectorMetadata)

-- | The connector metadata specific to Salesforce.
connectorMetadata_salesforce :: Lens.Lens' ConnectorMetadata (Prelude.Maybe SalesforceMetadata)
connectorMetadata_salesforce = Lens.lens (\ConnectorMetadata' {salesforce} -> salesforce) (\s@ConnectorMetadata' {} a -> s {salesforce = a} :: ConnectorMetadata)

-- | The connector metadata specific to Zendesk.
connectorMetadata_zendesk :: Lens.Lens' ConnectorMetadata (Prelude.Maybe ZendeskMetadata)
connectorMetadata_zendesk = Lens.lens (\ConnectorMetadata' {zendesk} -> zendesk) (\s@ConnectorMetadata' {} a -> s {zendesk = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon S3.
connectorMetadata_s3 :: Lens.Lens' ConnectorMetadata (Prelude.Maybe S3Metadata)
connectorMetadata_s3 = Lens.lens (\ConnectorMetadata' {s3} -> s3) (\s@ConnectorMetadata' {} a -> s {s3 = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon EventBridge.
connectorMetadata_eventBridge :: Lens.Lens' ConnectorMetadata (Prelude.Maybe EventBridgeMetadata)
connectorMetadata_eventBridge = Lens.lens (\ConnectorMetadata' {eventBridge} -> eventBridge) (\s@ConnectorMetadata' {} a -> s {eventBridge = a} :: ConnectorMetadata)

-- | The connector metadata specific to Trend Micro.
connectorMetadata_trendmicro :: Lens.Lens' ConnectorMetadata (Prelude.Maybe TrendmicroMetadata)
connectorMetadata_trendmicro = Lens.lens (\ConnectorMetadata' {trendmicro} -> trendmicro) (\s@ConnectorMetadata' {} a -> s {trendmicro = a} :: ConnectorMetadata)

-- | The connector metadata specific to Amazon Redshift.
connectorMetadata_redshift :: Lens.Lens' ConnectorMetadata (Prelude.Maybe RedshiftMetadata)
connectorMetadata_redshift = Lens.lens (\ConnectorMetadata' {redshift} -> redshift) (\s@ConnectorMetadata' {} a -> s {redshift = a} :: ConnectorMetadata)

-- | The connector metadata specific to Veeva.
connectorMetadata_veeva :: Lens.Lens' ConnectorMetadata (Prelude.Maybe VeevaMetadata)
connectorMetadata_veeva = Lens.lens (\ConnectorMetadata' {veeva} -> veeva) (\s@ConnectorMetadata' {} a -> s {veeva = a} :: ConnectorMetadata)

instance Core.FromJSON ConnectorMetadata where
  parseJSON =
    Core.withObject
      "ConnectorMetadata"
      ( \x ->
          ConnectorMetadata'
            Prelude.<$> (x Core..:? "Upsolver")
            Prelude.<*> (x Core..:? "Snowflake")
            Prelude.<*> (x Core..:? "Honeycode")
            Prelude.<*> (x Core..:? "ServiceNow")
            Prelude.<*> (x Core..:? "Dynatrace")
            Prelude.<*> (x Core..:? "Marketo")
            Prelude.<*> (x Core..:? "Slack")
            Prelude.<*> (x Core..:? "Singular")
            Prelude.<*> (x Core..:? "InforNexus")
            Prelude.<*> (x Core..:? "Amplitude")
            Prelude.<*> (x Core..:? "CustomerProfiles")
            Prelude.<*> (x Core..:? "Datadog")
            Prelude.<*> (x Core..:? "GoogleAnalytics")
            Prelude.<*> (x Core..:? "SAPOData")
            Prelude.<*> (x Core..:? "Salesforce")
            Prelude.<*> (x Core..:? "Zendesk")
            Prelude.<*> (x Core..:? "S3")
            Prelude.<*> (x Core..:? "EventBridge")
            Prelude.<*> (x Core..:? "Trendmicro")
            Prelude.<*> (x Core..:? "Redshift")
            Prelude.<*> (x Core..:? "Veeva")
      )

instance Prelude.Hashable ConnectorMetadata where
  hashWithSalt salt' ConnectorMetadata' {..} =
    salt' `Prelude.hashWithSalt` veeva
      `Prelude.hashWithSalt` redshift
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` eventBridge
      `Prelude.hashWithSalt` s3
      `Prelude.hashWithSalt` zendesk
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` sAPOData
      `Prelude.hashWithSalt` googleAnalytics
      `Prelude.hashWithSalt` datadog
      `Prelude.hashWithSalt` customerProfiles
      `Prelude.hashWithSalt` amplitude
      `Prelude.hashWithSalt` inforNexus
      `Prelude.hashWithSalt` singular
      `Prelude.hashWithSalt` slack
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` dynatrace
      `Prelude.hashWithSalt` serviceNow
      `Prelude.hashWithSalt` honeycode
      `Prelude.hashWithSalt` snowflake
      `Prelude.hashWithSalt` upsolver

instance Prelude.NFData ConnectorMetadata where
  rnf ConnectorMetadata' {..} =
    Prelude.rnf upsolver
      `Prelude.seq` Prelude.rnf veeva
      `Prelude.seq` Prelude.rnf redshift
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf eventBridge
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf zendesk
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf googleAnalytics
      `Prelude.seq` Prelude.rnf datadog
      `Prelude.seq` Prelude.rnf customerProfiles
      `Prelude.seq` Prelude.rnf amplitude
      `Prelude.seq` Prelude.rnf inforNexus
      `Prelude.seq` Prelude.rnf singular
      `Prelude.seq` Prelude.rnf slack
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf dynatrace
      `Prelude.seq` Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf honeycode
      `Prelude.seq` Prelude.rnf snowflake
