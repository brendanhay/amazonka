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
-- Module      : Amazonka.AppFlow.Types.SourceConnectorProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SourceConnectorProperties where

import Amazonka.AppFlow.Types.AmplitudeSourceProperties
import Amazonka.AppFlow.Types.CustomConnectorSourceProperties
import Amazonka.AppFlow.Types.DatadogSourceProperties
import Amazonka.AppFlow.Types.DynatraceSourceProperties
import Amazonka.AppFlow.Types.GoogleAnalyticsSourceProperties
import Amazonka.AppFlow.Types.InforNexusSourceProperties
import Amazonka.AppFlow.Types.MarketoSourceProperties
import Amazonka.AppFlow.Types.S3SourceProperties
import Amazonka.AppFlow.Types.SAPODataSourceProperties
import Amazonka.AppFlow.Types.SalesforceSourceProperties
import Amazonka.AppFlow.Types.ServiceNowSourceProperties
import Amazonka.AppFlow.Types.SingularSourceProperties
import Amazonka.AppFlow.Types.SlackSourceProperties
import Amazonka.AppFlow.Types.TrendmicroSourceProperties
import Amazonka.AppFlow.Types.VeevaSourceProperties
import Amazonka.AppFlow.Types.ZendeskSourceProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the information that is required to query a particular
-- connector.
--
-- /See:/ 'newSourceConnectorProperties' smart constructor.
data SourceConnectorProperties = SourceConnectorProperties'
  { -- | Specifies the information that is required for querying Amplitude.
    amplitude :: Prelude.Maybe AmplitudeSourceProperties,
    customConnector :: Prelude.Maybe CustomConnectorSourceProperties,
    -- | Specifies the information that is required for querying Datadog.
    datadog :: Prelude.Maybe DatadogSourceProperties,
    -- | Specifies the information that is required for querying Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceSourceProperties,
    -- | Specifies the information that is required for querying Google
    -- Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsSourceProperties,
    -- | Specifies the information that is required for querying Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusSourceProperties,
    -- | Specifies the information that is required for querying Marketo.
    marketo :: Prelude.Maybe MarketoSourceProperties,
    -- | Specifies the information that is required for querying Amazon S3.
    s3 :: Prelude.Maybe S3SourceProperties,
    sAPOData :: Prelude.Maybe SAPODataSourceProperties,
    -- | Specifies the information that is required for querying Salesforce.
    salesforce :: Prelude.Maybe SalesforceSourceProperties,
    -- | Specifies the information that is required for querying ServiceNow.
    serviceNow :: Prelude.Maybe ServiceNowSourceProperties,
    -- | Specifies the information that is required for querying Singular.
    singular :: Prelude.Maybe SingularSourceProperties,
    -- | Specifies the information that is required for querying Slack.
    slack :: Prelude.Maybe SlackSourceProperties,
    -- | Specifies the information that is required for querying Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroSourceProperties,
    -- | Specifies the information that is required for querying Veeva.
    veeva :: Prelude.Maybe VeevaSourceProperties,
    -- | Specifies the information that is required for querying Zendesk.
    zendesk :: Prelude.Maybe ZendeskSourceProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConnectorProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amplitude', 'sourceConnectorProperties_amplitude' - Specifies the information that is required for querying Amplitude.
--
-- 'customConnector', 'sourceConnectorProperties_customConnector' - Undocumented member.
--
-- 'datadog', 'sourceConnectorProperties_datadog' - Specifies the information that is required for querying Datadog.
--
-- 'dynatrace', 'sourceConnectorProperties_dynatrace' - Specifies the information that is required for querying Dynatrace.
--
-- 'googleAnalytics', 'sourceConnectorProperties_googleAnalytics' - Specifies the information that is required for querying Google
-- Analytics.
--
-- 'inforNexus', 'sourceConnectorProperties_inforNexus' - Specifies the information that is required for querying Infor Nexus.
--
-- 'marketo', 'sourceConnectorProperties_marketo' - Specifies the information that is required for querying Marketo.
--
-- 's3', 'sourceConnectorProperties_s3' - Specifies the information that is required for querying Amazon S3.
--
-- 'sAPOData', 'sourceConnectorProperties_sAPOData' - Undocumented member.
--
-- 'salesforce', 'sourceConnectorProperties_salesforce' - Specifies the information that is required for querying Salesforce.
--
-- 'serviceNow', 'sourceConnectorProperties_serviceNow' - Specifies the information that is required for querying ServiceNow.
--
-- 'singular', 'sourceConnectorProperties_singular' - Specifies the information that is required for querying Singular.
--
-- 'slack', 'sourceConnectorProperties_slack' - Specifies the information that is required for querying Slack.
--
-- 'trendmicro', 'sourceConnectorProperties_trendmicro' - Specifies the information that is required for querying Trend Micro.
--
-- 'veeva', 'sourceConnectorProperties_veeva' - Specifies the information that is required for querying Veeva.
--
-- 'zendesk', 'sourceConnectorProperties_zendesk' - Specifies the information that is required for querying Zendesk.
newSourceConnectorProperties ::
  SourceConnectorProperties
newSourceConnectorProperties =
  SourceConnectorProperties'
    { amplitude =
        Prelude.Nothing,
      customConnector = Prelude.Nothing,
      datadog = Prelude.Nothing,
      dynatrace = Prelude.Nothing,
      googleAnalytics = Prelude.Nothing,
      inforNexus = Prelude.Nothing,
      marketo = Prelude.Nothing,
      s3 = Prelude.Nothing,
      sAPOData = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      serviceNow = Prelude.Nothing,
      singular = Prelude.Nothing,
      slack = Prelude.Nothing,
      trendmicro = Prelude.Nothing,
      veeva = Prelude.Nothing,
      zendesk = Prelude.Nothing
    }

-- | Specifies the information that is required for querying Amplitude.
sourceConnectorProperties_amplitude :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe AmplitudeSourceProperties)
sourceConnectorProperties_amplitude = Lens.lens (\SourceConnectorProperties' {amplitude} -> amplitude) (\s@SourceConnectorProperties' {} a -> s {amplitude = a} :: SourceConnectorProperties)

-- | Undocumented member.
sourceConnectorProperties_customConnector :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe CustomConnectorSourceProperties)
sourceConnectorProperties_customConnector = Lens.lens (\SourceConnectorProperties' {customConnector} -> customConnector) (\s@SourceConnectorProperties' {} a -> s {customConnector = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Datadog.
sourceConnectorProperties_datadog :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe DatadogSourceProperties)
sourceConnectorProperties_datadog = Lens.lens (\SourceConnectorProperties' {datadog} -> datadog) (\s@SourceConnectorProperties' {} a -> s {datadog = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Dynatrace.
sourceConnectorProperties_dynatrace :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe DynatraceSourceProperties)
sourceConnectorProperties_dynatrace = Lens.lens (\SourceConnectorProperties' {dynatrace} -> dynatrace) (\s@SourceConnectorProperties' {} a -> s {dynatrace = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Google
-- Analytics.
sourceConnectorProperties_googleAnalytics :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe GoogleAnalyticsSourceProperties)
sourceConnectorProperties_googleAnalytics = Lens.lens (\SourceConnectorProperties' {googleAnalytics} -> googleAnalytics) (\s@SourceConnectorProperties' {} a -> s {googleAnalytics = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Infor Nexus.
sourceConnectorProperties_inforNexus :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe InforNexusSourceProperties)
sourceConnectorProperties_inforNexus = Lens.lens (\SourceConnectorProperties' {inforNexus} -> inforNexus) (\s@SourceConnectorProperties' {} a -> s {inforNexus = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Marketo.
sourceConnectorProperties_marketo :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe MarketoSourceProperties)
sourceConnectorProperties_marketo = Lens.lens (\SourceConnectorProperties' {marketo} -> marketo) (\s@SourceConnectorProperties' {} a -> s {marketo = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Amazon S3.
sourceConnectorProperties_s3 :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe S3SourceProperties)
sourceConnectorProperties_s3 = Lens.lens (\SourceConnectorProperties' {s3} -> s3) (\s@SourceConnectorProperties' {} a -> s {s3 = a} :: SourceConnectorProperties)

-- | Undocumented member.
sourceConnectorProperties_sAPOData :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe SAPODataSourceProperties)
sourceConnectorProperties_sAPOData = Lens.lens (\SourceConnectorProperties' {sAPOData} -> sAPOData) (\s@SourceConnectorProperties' {} a -> s {sAPOData = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Salesforce.
sourceConnectorProperties_salesforce :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe SalesforceSourceProperties)
sourceConnectorProperties_salesforce = Lens.lens (\SourceConnectorProperties' {salesforce} -> salesforce) (\s@SourceConnectorProperties' {} a -> s {salesforce = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying ServiceNow.
sourceConnectorProperties_serviceNow :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe ServiceNowSourceProperties)
sourceConnectorProperties_serviceNow = Lens.lens (\SourceConnectorProperties' {serviceNow} -> serviceNow) (\s@SourceConnectorProperties' {} a -> s {serviceNow = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Singular.
sourceConnectorProperties_singular :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe SingularSourceProperties)
sourceConnectorProperties_singular = Lens.lens (\SourceConnectorProperties' {singular} -> singular) (\s@SourceConnectorProperties' {} a -> s {singular = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Slack.
sourceConnectorProperties_slack :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe SlackSourceProperties)
sourceConnectorProperties_slack = Lens.lens (\SourceConnectorProperties' {slack} -> slack) (\s@SourceConnectorProperties' {} a -> s {slack = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Trend Micro.
sourceConnectorProperties_trendmicro :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe TrendmicroSourceProperties)
sourceConnectorProperties_trendmicro = Lens.lens (\SourceConnectorProperties' {trendmicro} -> trendmicro) (\s@SourceConnectorProperties' {} a -> s {trendmicro = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Veeva.
sourceConnectorProperties_veeva :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe VeevaSourceProperties)
sourceConnectorProperties_veeva = Lens.lens (\SourceConnectorProperties' {veeva} -> veeva) (\s@SourceConnectorProperties' {} a -> s {veeva = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Zendesk.
sourceConnectorProperties_zendesk :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe ZendeskSourceProperties)
sourceConnectorProperties_zendesk = Lens.lens (\SourceConnectorProperties' {zendesk} -> zendesk) (\s@SourceConnectorProperties' {} a -> s {zendesk = a} :: SourceConnectorProperties)

instance Data.FromJSON SourceConnectorProperties where
  parseJSON =
    Data.withObject
      "SourceConnectorProperties"
      ( \x ->
          SourceConnectorProperties'
            Prelude.<$> (x Data..:? "Amplitude")
            Prelude.<*> (x Data..:? "CustomConnector")
            Prelude.<*> (x Data..:? "Datadog")
            Prelude.<*> (x Data..:? "Dynatrace")
            Prelude.<*> (x Data..:? "GoogleAnalytics")
            Prelude.<*> (x Data..:? "InforNexus")
            Prelude.<*> (x Data..:? "Marketo")
            Prelude.<*> (x Data..:? "S3")
            Prelude.<*> (x Data..:? "SAPOData")
            Prelude.<*> (x Data..:? "Salesforce")
            Prelude.<*> (x Data..:? "ServiceNow")
            Prelude.<*> (x Data..:? "Singular")
            Prelude.<*> (x Data..:? "Slack")
            Prelude.<*> (x Data..:? "Trendmicro")
            Prelude.<*> (x Data..:? "Veeva")
            Prelude.<*> (x Data..:? "Zendesk")
      )

instance Prelude.Hashable SourceConnectorProperties where
  hashWithSalt _salt SourceConnectorProperties' {..} =
    _salt
      `Prelude.hashWithSalt` amplitude
      `Prelude.hashWithSalt` customConnector
      `Prelude.hashWithSalt` datadog
      `Prelude.hashWithSalt` dynatrace
      `Prelude.hashWithSalt` googleAnalytics
      `Prelude.hashWithSalt` inforNexus
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` s3
      `Prelude.hashWithSalt` sAPOData
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` serviceNow
      `Prelude.hashWithSalt` singular
      `Prelude.hashWithSalt` slack
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` veeva
      `Prelude.hashWithSalt` zendesk

instance Prelude.NFData SourceConnectorProperties where
  rnf SourceConnectorProperties' {..} =
    Prelude.rnf amplitude
      `Prelude.seq` Prelude.rnf customConnector
      `Prelude.seq` Prelude.rnf datadog
      `Prelude.seq` Prelude.rnf dynatrace
      `Prelude.seq` Prelude.rnf googleAnalytics
      `Prelude.seq` Prelude.rnf inforNexus
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf singular
      `Prelude.seq` Prelude.rnf slack
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf veeva
      `Prelude.seq` Prelude.rnf zendesk

instance Data.ToJSON SourceConnectorProperties where
  toJSON SourceConnectorProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Amplitude" Data..=) Prelude.<$> amplitude,
            ("CustomConnector" Data..=)
              Prelude.<$> customConnector,
            ("Datadog" Data..=) Prelude.<$> datadog,
            ("Dynatrace" Data..=) Prelude.<$> dynatrace,
            ("GoogleAnalytics" Data..=)
              Prelude.<$> googleAnalytics,
            ("InforNexus" Data..=) Prelude.<$> inforNexus,
            ("Marketo" Data..=) Prelude.<$> marketo,
            ("S3" Data..=) Prelude.<$> s3,
            ("SAPOData" Data..=) Prelude.<$> sAPOData,
            ("Salesforce" Data..=) Prelude.<$> salesforce,
            ("ServiceNow" Data..=) Prelude.<$> serviceNow,
            ("Singular" Data..=) Prelude.<$> singular,
            ("Slack" Data..=) Prelude.<$> slack,
            ("Trendmicro" Data..=) Prelude.<$> trendmicro,
            ("Veeva" Data..=) Prelude.<$> veeva,
            ("Zendesk" Data..=) Prelude.<$> zendesk
          ]
      )
