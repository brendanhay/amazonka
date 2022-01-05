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
-- Module      : Amazonka.AppFlow.Types.ConnectorProfileProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorProfileProperties where

import Amazonka.AppFlow.Types.AmplitudeConnectorProfileProperties
import Amazonka.AppFlow.Types.DatadogConnectorProfileProperties
import Amazonka.AppFlow.Types.DynatraceConnectorProfileProperties
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileProperties
import Amazonka.AppFlow.Types.HoneycodeConnectorProfileProperties
import Amazonka.AppFlow.Types.InforNexusConnectorProfileProperties
import Amazonka.AppFlow.Types.MarketoConnectorProfileProperties
import Amazonka.AppFlow.Types.RedshiftConnectorProfileProperties
import Amazonka.AppFlow.Types.SAPODataConnectorProfileProperties
import Amazonka.AppFlow.Types.SalesforceConnectorProfileProperties
import Amazonka.AppFlow.Types.ServiceNowConnectorProfileProperties
import Amazonka.AppFlow.Types.SingularConnectorProfileProperties
import Amazonka.AppFlow.Types.SlackConnectorProfileProperties
import Amazonka.AppFlow.Types.SnowflakeConnectorProfileProperties
import Amazonka.AppFlow.Types.TrendmicroConnectorProfileProperties
import Amazonka.AppFlow.Types.VeevaConnectorProfileProperties
import Amazonka.AppFlow.Types.ZendeskConnectorProfileProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required by each connector.
--
-- /See:/ 'newConnectorProfileProperties' smart constructor.
data ConnectorProfileProperties = ConnectorProfileProperties'
  { -- | The connector-specific properties required by Snowflake.
    snowflake :: Prelude.Maybe SnowflakeConnectorProfileProperties,
    -- | The connector-specific properties required by Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeConnectorProfileProperties,
    -- | The connector-specific properties required by serviceNow.
    serviceNow :: Prelude.Maybe ServiceNowConnectorProfileProperties,
    -- | The connector-specific properties required by Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceConnectorProfileProperties,
    -- | The connector-specific properties required by Marketo.
    marketo :: Prelude.Maybe MarketoConnectorProfileProperties,
    -- | The connector-specific properties required by Slack.
    slack :: Prelude.Maybe SlackConnectorProfileProperties,
    -- | The connector-specific properties required by Singular.
    singular :: Prelude.Maybe SingularConnectorProfileProperties,
    -- | The connector-specific properties required by Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusConnectorProfileProperties,
    -- | The connector-specific properties required by Amplitude.
    amplitude :: Prelude.Maybe AmplitudeConnectorProfileProperties,
    -- | The connector-specific properties required by Datadog.
    datadog :: Prelude.Maybe DatadogConnectorProfileProperties,
    -- | The connector-specific properties required Google Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsConnectorProfileProperties,
    sAPOData :: Prelude.Maybe SAPODataConnectorProfileProperties,
    -- | The connector-specific properties required by Salesforce.
    salesforce :: Prelude.Maybe SalesforceConnectorProfileProperties,
    -- | The connector-specific properties required by Zendesk.
    zendesk :: Prelude.Maybe ZendeskConnectorProfileProperties,
    -- | The connector-specific properties required by Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroConnectorProfileProperties,
    -- | The connector-specific properties required by Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftConnectorProfileProperties,
    -- | The connector-specific properties required by Veeva.
    veeva :: Prelude.Maybe VeevaConnectorProfileProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snowflake', 'connectorProfileProperties_snowflake' - The connector-specific properties required by Snowflake.
--
-- 'honeycode', 'connectorProfileProperties_honeycode' - The connector-specific properties required by Amazon Honeycode.
--
-- 'serviceNow', 'connectorProfileProperties_serviceNow' - The connector-specific properties required by serviceNow.
--
-- 'dynatrace', 'connectorProfileProperties_dynatrace' - The connector-specific properties required by Dynatrace.
--
-- 'marketo', 'connectorProfileProperties_marketo' - The connector-specific properties required by Marketo.
--
-- 'slack', 'connectorProfileProperties_slack' - The connector-specific properties required by Slack.
--
-- 'singular', 'connectorProfileProperties_singular' - The connector-specific properties required by Singular.
--
-- 'inforNexus', 'connectorProfileProperties_inforNexus' - The connector-specific properties required by Infor Nexus.
--
-- 'amplitude', 'connectorProfileProperties_amplitude' - The connector-specific properties required by Amplitude.
--
-- 'datadog', 'connectorProfileProperties_datadog' - The connector-specific properties required by Datadog.
--
-- 'googleAnalytics', 'connectorProfileProperties_googleAnalytics' - The connector-specific properties required Google Analytics.
--
-- 'sAPOData', 'connectorProfileProperties_sAPOData' - Undocumented member.
--
-- 'salesforce', 'connectorProfileProperties_salesforce' - The connector-specific properties required by Salesforce.
--
-- 'zendesk', 'connectorProfileProperties_zendesk' - The connector-specific properties required by Zendesk.
--
-- 'trendmicro', 'connectorProfileProperties_trendmicro' - The connector-specific properties required by Trend Micro.
--
-- 'redshift', 'connectorProfileProperties_redshift' - The connector-specific properties required by Amazon Redshift.
--
-- 'veeva', 'connectorProfileProperties_veeva' - The connector-specific properties required by Veeva.
newConnectorProfileProperties ::
  ConnectorProfileProperties
newConnectorProfileProperties =
  ConnectorProfileProperties'
    { snowflake =
        Prelude.Nothing,
      honeycode = Prelude.Nothing,
      serviceNow = Prelude.Nothing,
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
      trendmicro = Prelude.Nothing,
      redshift = Prelude.Nothing,
      veeva = Prelude.Nothing
    }

-- | The connector-specific properties required by Snowflake.
connectorProfileProperties_snowflake :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SnowflakeConnectorProfileProperties)
connectorProfileProperties_snowflake = Lens.lens (\ConnectorProfileProperties' {snowflake} -> snowflake) (\s@ConnectorProfileProperties' {} a -> s {snowflake = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Amazon Honeycode.
connectorProfileProperties_honeycode :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe HoneycodeConnectorProfileProperties)
connectorProfileProperties_honeycode = Lens.lens (\ConnectorProfileProperties' {honeycode} -> honeycode) (\s@ConnectorProfileProperties' {} a -> s {honeycode = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by serviceNow.
connectorProfileProperties_serviceNow :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe ServiceNowConnectorProfileProperties)
connectorProfileProperties_serviceNow = Lens.lens (\ConnectorProfileProperties' {serviceNow} -> serviceNow) (\s@ConnectorProfileProperties' {} a -> s {serviceNow = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Dynatrace.
connectorProfileProperties_dynatrace :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe DynatraceConnectorProfileProperties)
connectorProfileProperties_dynatrace = Lens.lens (\ConnectorProfileProperties' {dynatrace} -> dynatrace) (\s@ConnectorProfileProperties' {} a -> s {dynatrace = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Marketo.
connectorProfileProperties_marketo :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe MarketoConnectorProfileProperties)
connectorProfileProperties_marketo = Lens.lens (\ConnectorProfileProperties' {marketo} -> marketo) (\s@ConnectorProfileProperties' {} a -> s {marketo = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Slack.
connectorProfileProperties_slack :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SlackConnectorProfileProperties)
connectorProfileProperties_slack = Lens.lens (\ConnectorProfileProperties' {slack} -> slack) (\s@ConnectorProfileProperties' {} a -> s {slack = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Singular.
connectorProfileProperties_singular :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SingularConnectorProfileProperties)
connectorProfileProperties_singular = Lens.lens (\ConnectorProfileProperties' {singular} -> singular) (\s@ConnectorProfileProperties' {} a -> s {singular = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Infor Nexus.
connectorProfileProperties_inforNexus :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe InforNexusConnectorProfileProperties)
connectorProfileProperties_inforNexus = Lens.lens (\ConnectorProfileProperties' {inforNexus} -> inforNexus) (\s@ConnectorProfileProperties' {} a -> s {inforNexus = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Amplitude.
connectorProfileProperties_amplitude :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe AmplitudeConnectorProfileProperties)
connectorProfileProperties_amplitude = Lens.lens (\ConnectorProfileProperties' {amplitude} -> amplitude) (\s@ConnectorProfileProperties' {} a -> s {amplitude = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Datadog.
connectorProfileProperties_datadog :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe DatadogConnectorProfileProperties)
connectorProfileProperties_datadog = Lens.lens (\ConnectorProfileProperties' {datadog} -> datadog) (\s@ConnectorProfileProperties' {} a -> s {datadog = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required Google Analytics.
connectorProfileProperties_googleAnalytics :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe GoogleAnalyticsConnectorProfileProperties)
connectorProfileProperties_googleAnalytics = Lens.lens (\ConnectorProfileProperties' {googleAnalytics} -> googleAnalytics) (\s@ConnectorProfileProperties' {} a -> s {googleAnalytics = a} :: ConnectorProfileProperties)

-- | Undocumented member.
connectorProfileProperties_sAPOData :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SAPODataConnectorProfileProperties)
connectorProfileProperties_sAPOData = Lens.lens (\ConnectorProfileProperties' {sAPOData} -> sAPOData) (\s@ConnectorProfileProperties' {} a -> s {sAPOData = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Salesforce.
connectorProfileProperties_salesforce :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SalesforceConnectorProfileProperties)
connectorProfileProperties_salesforce = Lens.lens (\ConnectorProfileProperties' {salesforce} -> salesforce) (\s@ConnectorProfileProperties' {} a -> s {salesforce = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Zendesk.
connectorProfileProperties_zendesk :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe ZendeskConnectorProfileProperties)
connectorProfileProperties_zendesk = Lens.lens (\ConnectorProfileProperties' {zendesk} -> zendesk) (\s@ConnectorProfileProperties' {} a -> s {zendesk = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Trend Micro.
connectorProfileProperties_trendmicro :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe TrendmicroConnectorProfileProperties)
connectorProfileProperties_trendmicro = Lens.lens (\ConnectorProfileProperties' {trendmicro} -> trendmicro) (\s@ConnectorProfileProperties' {} a -> s {trendmicro = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Amazon Redshift.
connectorProfileProperties_redshift :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe RedshiftConnectorProfileProperties)
connectorProfileProperties_redshift = Lens.lens (\ConnectorProfileProperties' {redshift} -> redshift) (\s@ConnectorProfileProperties' {} a -> s {redshift = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Veeva.
connectorProfileProperties_veeva :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe VeevaConnectorProfileProperties)
connectorProfileProperties_veeva = Lens.lens (\ConnectorProfileProperties' {veeva} -> veeva) (\s@ConnectorProfileProperties' {} a -> s {veeva = a} :: ConnectorProfileProperties)

instance Core.FromJSON ConnectorProfileProperties where
  parseJSON =
    Core.withObject
      "ConnectorProfileProperties"
      ( \x ->
          ConnectorProfileProperties'
            Prelude.<$> (x Core..:? "Snowflake")
            Prelude.<*> (x Core..:? "Honeycode")
            Prelude.<*> (x Core..:? "ServiceNow")
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
            Prelude.<*> (x Core..:? "Trendmicro")
            Prelude.<*> (x Core..:? "Redshift")
            Prelude.<*> (x Core..:? "Veeva")
      )

instance Prelude.Hashable ConnectorProfileProperties where
  hashWithSalt _salt ConnectorProfileProperties' {..} =
    _salt `Prelude.hashWithSalt` snowflake
      `Prelude.hashWithSalt` honeycode
      `Prelude.hashWithSalt` serviceNow
      `Prelude.hashWithSalt` dynatrace
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` slack
      `Prelude.hashWithSalt` singular
      `Prelude.hashWithSalt` inforNexus
      `Prelude.hashWithSalt` amplitude
      `Prelude.hashWithSalt` datadog
      `Prelude.hashWithSalt` googleAnalytics
      `Prelude.hashWithSalt` sAPOData
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` zendesk
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` redshift
      `Prelude.hashWithSalt` veeva

instance Prelude.NFData ConnectorProfileProperties where
  rnf ConnectorProfileProperties' {..} =
    Prelude.rnf snowflake
      `Prelude.seq` Prelude.rnf honeycode
      `Prelude.seq` Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf dynatrace
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf slack
      `Prelude.seq` Prelude.rnf singular
      `Prelude.seq` Prelude.rnf inforNexus
      `Prelude.seq` Prelude.rnf amplitude
      `Prelude.seq` Prelude.rnf datadog
      `Prelude.seq` Prelude.rnf googleAnalytics
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf zendesk
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf redshift
      `Prelude.seq` Prelude.rnf veeva

instance Core.ToJSON ConnectorProfileProperties where
  toJSON ConnectorProfileProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Snowflake" Core..=) Prelude.<$> snowflake,
            ("Honeycode" Core..=) Prelude.<$> honeycode,
            ("ServiceNow" Core..=) Prelude.<$> serviceNow,
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
            ("Trendmicro" Core..=) Prelude.<$> trendmicro,
            ("Redshift" Core..=) Prelude.<$> redshift,
            ("Veeva" Core..=) Prelude.<$> veeva
          ]
      )
