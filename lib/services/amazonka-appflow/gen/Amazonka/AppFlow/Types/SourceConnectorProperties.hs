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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SourceConnectorProperties where

import Amazonka.AppFlow.Types.AmplitudeSourceProperties
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the information that is required to query a particular
-- connector.
--
-- /See:/ 'newSourceConnectorProperties' smart constructor.
data SourceConnectorProperties = SourceConnectorProperties'
  { -- | Specifies the information that is required for querying ServiceNow.
    serviceNow :: Prelude.Maybe ServiceNowSourceProperties,
    -- | Specifies the information that is required for querying Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceSourceProperties,
    -- | Specifies the information that is required for querying Marketo.
    marketo :: Prelude.Maybe MarketoSourceProperties,
    -- | Specifies the information that is required for querying Slack.
    slack :: Prelude.Maybe SlackSourceProperties,
    -- | Specifies the information that is required for querying Singular.
    singular :: Prelude.Maybe SingularSourceProperties,
    -- | Specifies the information that is required for querying Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusSourceProperties,
    -- | Specifies the information that is required for querying Amplitude.
    amplitude :: Prelude.Maybe AmplitudeSourceProperties,
    -- | Specifies the information that is required for querying Datadog.
    datadog :: Prelude.Maybe DatadogSourceProperties,
    -- | Specifies the information that is required for querying Google
    -- Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsSourceProperties,
    sAPOData :: Prelude.Maybe SAPODataSourceProperties,
    -- | Specifies the information that is required for querying Salesforce.
    salesforce :: Prelude.Maybe SalesforceSourceProperties,
    -- | Specifies the information that is required for querying Zendesk.
    zendesk :: Prelude.Maybe ZendeskSourceProperties,
    -- | Specifies the information that is required for querying Amazon S3.
    s3 :: Prelude.Maybe S3SourceProperties,
    -- | Specifies the information that is required for querying Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroSourceProperties,
    -- | Specifies the information that is required for querying Veeva.
    veeva :: Prelude.Maybe VeevaSourceProperties
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
-- 'serviceNow', 'sourceConnectorProperties_serviceNow' - Specifies the information that is required for querying ServiceNow.
--
-- 'dynatrace', 'sourceConnectorProperties_dynatrace' - Specifies the information that is required for querying Dynatrace.
--
-- 'marketo', 'sourceConnectorProperties_marketo' - Specifies the information that is required for querying Marketo.
--
-- 'slack', 'sourceConnectorProperties_slack' - Specifies the information that is required for querying Slack.
--
-- 'singular', 'sourceConnectorProperties_singular' - Specifies the information that is required for querying Singular.
--
-- 'inforNexus', 'sourceConnectorProperties_inforNexus' - Specifies the information that is required for querying Infor Nexus.
--
-- 'amplitude', 'sourceConnectorProperties_amplitude' - Specifies the information that is required for querying Amplitude.
--
-- 'datadog', 'sourceConnectorProperties_datadog' - Specifies the information that is required for querying Datadog.
--
-- 'googleAnalytics', 'sourceConnectorProperties_googleAnalytics' - Specifies the information that is required for querying Google
-- Analytics.
--
-- 'sAPOData', 'sourceConnectorProperties_sAPOData' - Undocumented member.
--
-- 'salesforce', 'sourceConnectorProperties_salesforce' - Specifies the information that is required for querying Salesforce.
--
-- 'zendesk', 'sourceConnectorProperties_zendesk' - Specifies the information that is required for querying Zendesk.
--
-- 's3', 'sourceConnectorProperties_s3' - Specifies the information that is required for querying Amazon S3.
--
-- 'trendmicro', 'sourceConnectorProperties_trendmicro' - Specifies the information that is required for querying Trend Micro.
--
-- 'veeva', 'sourceConnectorProperties_veeva' - Specifies the information that is required for querying Veeva.
newSourceConnectorProperties ::
  SourceConnectorProperties
newSourceConnectorProperties =
  SourceConnectorProperties'
    { serviceNow =
        Prelude.Nothing,
      dynatrace = Prelude.Nothing,
      marketo = Prelude.Nothing,
      slack = Prelude.Nothing,
      singular = Prelude.Nothing,
      inforNexus = Prelude.Nothing,
      amplitude = Prelude.Nothing,
      datadog = Prelude.Nothing,
      googleAnalytics = Prelude.Nothing,
      sAPOData = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      zendesk = Prelude.Nothing,
      s3 = Prelude.Nothing,
      trendmicro = Prelude.Nothing,
      veeva = Prelude.Nothing
    }

-- | Specifies the information that is required for querying ServiceNow.
sourceConnectorProperties_serviceNow :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe ServiceNowSourceProperties)
sourceConnectorProperties_serviceNow = Lens.lens (\SourceConnectorProperties' {serviceNow} -> serviceNow) (\s@SourceConnectorProperties' {} a -> s {serviceNow = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Dynatrace.
sourceConnectorProperties_dynatrace :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe DynatraceSourceProperties)
sourceConnectorProperties_dynatrace = Lens.lens (\SourceConnectorProperties' {dynatrace} -> dynatrace) (\s@SourceConnectorProperties' {} a -> s {dynatrace = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Marketo.
sourceConnectorProperties_marketo :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe MarketoSourceProperties)
sourceConnectorProperties_marketo = Lens.lens (\SourceConnectorProperties' {marketo} -> marketo) (\s@SourceConnectorProperties' {} a -> s {marketo = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Slack.
sourceConnectorProperties_slack :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe SlackSourceProperties)
sourceConnectorProperties_slack = Lens.lens (\SourceConnectorProperties' {slack} -> slack) (\s@SourceConnectorProperties' {} a -> s {slack = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Singular.
sourceConnectorProperties_singular :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe SingularSourceProperties)
sourceConnectorProperties_singular = Lens.lens (\SourceConnectorProperties' {singular} -> singular) (\s@SourceConnectorProperties' {} a -> s {singular = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Infor Nexus.
sourceConnectorProperties_inforNexus :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe InforNexusSourceProperties)
sourceConnectorProperties_inforNexus = Lens.lens (\SourceConnectorProperties' {inforNexus} -> inforNexus) (\s@SourceConnectorProperties' {} a -> s {inforNexus = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Amplitude.
sourceConnectorProperties_amplitude :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe AmplitudeSourceProperties)
sourceConnectorProperties_amplitude = Lens.lens (\SourceConnectorProperties' {amplitude} -> amplitude) (\s@SourceConnectorProperties' {} a -> s {amplitude = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Datadog.
sourceConnectorProperties_datadog :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe DatadogSourceProperties)
sourceConnectorProperties_datadog = Lens.lens (\SourceConnectorProperties' {datadog} -> datadog) (\s@SourceConnectorProperties' {} a -> s {datadog = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Google
-- Analytics.
sourceConnectorProperties_googleAnalytics :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe GoogleAnalyticsSourceProperties)
sourceConnectorProperties_googleAnalytics = Lens.lens (\SourceConnectorProperties' {googleAnalytics} -> googleAnalytics) (\s@SourceConnectorProperties' {} a -> s {googleAnalytics = a} :: SourceConnectorProperties)

-- | Undocumented member.
sourceConnectorProperties_sAPOData :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe SAPODataSourceProperties)
sourceConnectorProperties_sAPOData = Lens.lens (\SourceConnectorProperties' {sAPOData} -> sAPOData) (\s@SourceConnectorProperties' {} a -> s {sAPOData = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Salesforce.
sourceConnectorProperties_salesforce :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe SalesforceSourceProperties)
sourceConnectorProperties_salesforce = Lens.lens (\SourceConnectorProperties' {salesforce} -> salesforce) (\s@SourceConnectorProperties' {} a -> s {salesforce = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Zendesk.
sourceConnectorProperties_zendesk :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe ZendeskSourceProperties)
sourceConnectorProperties_zendesk = Lens.lens (\SourceConnectorProperties' {zendesk} -> zendesk) (\s@SourceConnectorProperties' {} a -> s {zendesk = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Amazon S3.
sourceConnectorProperties_s3 :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe S3SourceProperties)
sourceConnectorProperties_s3 = Lens.lens (\SourceConnectorProperties' {s3} -> s3) (\s@SourceConnectorProperties' {} a -> s {s3 = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Trend Micro.
sourceConnectorProperties_trendmicro :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe TrendmicroSourceProperties)
sourceConnectorProperties_trendmicro = Lens.lens (\SourceConnectorProperties' {trendmicro} -> trendmicro) (\s@SourceConnectorProperties' {} a -> s {trendmicro = a} :: SourceConnectorProperties)

-- | Specifies the information that is required for querying Veeva.
sourceConnectorProperties_veeva :: Lens.Lens' SourceConnectorProperties (Prelude.Maybe VeevaSourceProperties)
sourceConnectorProperties_veeva = Lens.lens (\SourceConnectorProperties' {veeva} -> veeva) (\s@SourceConnectorProperties' {} a -> s {veeva = a} :: SourceConnectorProperties)

instance Core.FromJSON SourceConnectorProperties where
  parseJSON =
    Core.withObject
      "SourceConnectorProperties"
      ( \x ->
          SourceConnectorProperties'
            Prelude.<$> (x Core..:? "ServiceNow")
            Prelude.<*> (x Core..:? "Dynatrace")
            Prelude.<*> (x Core..:? "Marketo")
            Prelude.<*> (x Core..:? "Slack")
            Prelude.<*> (x Core..:? "Singular")
            Prelude.<*> (x Core..:? "InforNexus")
            Prelude.<*> (x Core..:? "Amplitude")
            Prelude.<*> (x Core..:? "Datadog")
            Prelude.<*> (x Core..:? "GoogleAnalytics")
            Prelude.<*> (x Core..:? "SAPOData")
            Prelude.<*> (x Core..:? "Salesforce")
            Prelude.<*> (x Core..:? "Zendesk")
            Prelude.<*> (x Core..:? "S3")
            Prelude.<*> (x Core..:? "Trendmicro")
            Prelude.<*> (x Core..:? "Veeva")
      )

instance Prelude.Hashable SourceConnectorProperties where
  hashWithSalt salt' SourceConnectorProperties' {..} =
    salt' `Prelude.hashWithSalt` veeva
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` s3
      `Prelude.hashWithSalt` zendesk
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` sAPOData
      `Prelude.hashWithSalt` googleAnalytics
      `Prelude.hashWithSalt` datadog
      `Prelude.hashWithSalt` amplitude
      `Prelude.hashWithSalt` inforNexus
      `Prelude.hashWithSalt` singular
      `Prelude.hashWithSalt` slack
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` dynatrace
      `Prelude.hashWithSalt` serviceNow

instance Prelude.NFData SourceConnectorProperties where
  rnf SourceConnectorProperties' {..} =
    Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf veeva
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf s3
      `Prelude.seq` Prelude.rnf zendesk
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf googleAnalytics
      `Prelude.seq` Prelude.rnf datadog
      `Prelude.seq` Prelude.rnf amplitude
      `Prelude.seq` Prelude.rnf inforNexus
      `Prelude.seq` Prelude.rnf singular
      `Prelude.seq` Prelude.rnf slack
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf dynatrace

instance Core.ToJSON SourceConnectorProperties where
  toJSON SourceConnectorProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServiceNow" Core..=) Prelude.<$> serviceNow,
            ("Dynatrace" Core..=) Prelude.<$> dynatrace,
            ("Marketo" Core..=) Prelude.<$> marketo,
            ("Slack" Core..=) Prelude.<$> slack,
            ("Singular" Core..=) Prelude.<$> singular,
            ("InforNexus" Core..=) Prelude.<$> inforNexus,
            ("Amplitude" Core..=) Prelude.<$> amplitude,
            ("Datadog" Core..=) Prelude.<$> datadog,
            ("GoogleAnalytics" Core..=)
              Prelude.<$> googleAnalytics,
            ("SAPOData" Core..=) Prelude.<$> sAPOData,
            ("Salesforce" Core..=) Prelude.<$> salesforce,
            ("Zendesk" Core..=) Prelude.<$> zendesk,
            ("S3" Core..=) Prelude.<$> s3,
            ("Trendmicro" Core..=) Prelude.<$> trendmicro,
            ("Veeva" Core..=) Prelude.<$> veeva
          ]
      )
