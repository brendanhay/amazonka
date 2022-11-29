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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorProfileProperties where

import Amazonka.AppFlow.Types.AmplitudeConnectorProfileProperties
import Amazonka.AppFlow.Types.CustomConnectorProfileProperties
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required by each connector.
--
-- /See:/ 'newConnectorProfileProperties' smart constructor.
data ConnectorProfileProperties = ConnectorProfileProperties'
  { -- | The connector-specific properties required by Zendesk.
    zendesk :: Prelude.Maybe ZendeskConnectorProfileProperties,
    -- | The connector-specific properties required by Slack.
    slack :: Prelude.Maybe SlackConnectorProfileProperties,
    -- | The connector-specific properties required by Singular.
    singular :: Prelude.Maybe SingularConnectorProfileProperties,
    -- | The connector-specific properties required by Veeva.
    veeva :: Prelude.Maybe VeevaConnectorProfileProperties,
    -- | The connector-specific properties required by Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeConnectorProfileProperties,
    -- | The connector-specific properties required by Salesforce.
    salesforce :: Prelude.Maybe SalesforceConnectorProfileProperties,
    -- | The connector-specific properties required by Snowflake.
    snowflake :: Prelude.Maybe SnowflakeConnectorProfileProperties,
    sAPOData :: Prelude.Maybe SAPODataConnectorProfileProperties,
    -- | The connector-specific properties required by Marketo.
    marketo :: Prelude.Maybe MarketoConnectorProfileProperties,
    -- | The connector-specific properties required by Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftConnectorProfileProperties,
    -- | The connector-specific properties required by Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroConnectorProfileProperties,
    -- | The connector-specific properties required by Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusConnectorProfileProperties,
    -- | The connector-specific properties required by serviceNow.
    serviceNow :: Prelude.Maybe ServiceNowConnectorProfileProperties,
    -- | The connector-specific properties required by Datadog.
    datadog :: Prelude.Maybe DatadogConnectorProfileProperties,
    -- | The properties required by the custom connector.
    customConnector :: Prelude.Maybe CustomConnectorProfileProperties,
    -- | The connector-specific properties required by Amplitude.
    amplitude :: Prelude.Maybe AmplitudeConnectorProfileProperties,
    -- | The connector-specific properties required by Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceConnectorProfileProperties,
    -- | The connector-specific properties required Google Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsConnectorProfileProperties
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
-- 'zendesk', 'connectorProfileProperties_zendesk' - The connector-specific properties required by Zendesk.
--
-- 'slack', 'connectorProfileProperties_slack' - The connector-specific properties required by Slack.
--
-- 'singular', 'connectorProfileProperties_singular' - The connector-specific properties required by Singular.
--
-- 'veeva', 'connectorProfileProperties_veeva' - The connector-specific properties required by Veeva.
--
-- 'honeycode', 'connectorProfileProperties_honeycode' - The connector-specific properties required by Amazon Honeycode.
--
-- 'salesforce', 'connectorProfileProperties_salesforce' - The connector-specific properties required by Salesforce.
--
-- 'snowflake', 'connectorProfileProperties_snowflake' - The connector-specific properties required by Snowflake.
--
-- 'sAPOData', 'connectorProfileProperties_sAPOData' - Undocumented member.
--
-- 'marketo', 'connectorProfileProperties_marketo' - The connector-specific properties required by Marketo.
--
-- 'redshift', 'connectorProfileProperties_redshift' - The connector-specific properties required by Amazon Redshift.
--
-- 'trendmicro', 'connectorProfileProperties_trendmicro' - The connector-specific properties required by Trend Micro.
--
-- 'inforNexus', 'connectorProfileProperties_inforNexus' - The connector-specific properties required by Infor Nexus.
--
-- 'serviceNow', 'connectorProfileProperties_serviceNow' - The connector-specific properties required by serviceNow.
--
-- 'datadog', 'connectorProfileProperties_datadog' - The connector-specific properties required by Datadog.
--
-- 'customConnector', 'connectorProfileProperties_customConnector' - The properties required by the custom connector.
--
-- 'amplitude', 'connectorProfileProperties_amplitude' - The connector-specific properties required by Amplitude.
--
-- 'dynatrace', 'connectorProfileProperties_dynatrace' - The connector-specific properties required by Dynatrace.
--
-- 'googleAnalytics', 'connectorProfileProperties_googleAnalytics' - The connector-specific properties required Google Analytics.
newConnectorProfileProperties ::
  ConnectorProfileProperties
newConnectorProfileProperties =
  ConnectorProfileProperties'
    { zendesk =
        Prelude.Nothing,
      slack = Prelude.Nothing,
      singular = Prelude.Nothing,
      veeva = Prelude.Nothing,
      honeycode = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      snowflake = Prelude.Nothing,
      sAPOData = Prelude.Nothing,
      marketo = Prelude.Nothing,
      redshift = Prelude.Nothing,
      trendmicro = Prelude.Nothing,
      inforNexus = Prelude.Nothing,
      serviceNow = Prelude.Nothing,
      datadog = Prelude.Nothing,
      customConnector = Prelude.Nothing,
      amplitude = Prelude.Nothing,
      dynatrace = Prelude.Nothing,
      googleAnalytics = Prelude.Nothing
    }

-- | The connector-specific properties required by Zendesk.
connectorProfileProperties_zendesk :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe ZendeskConnectorProfileProperties)
connectorProfileProperties_zendesk = Lens.lens (\ConnectorProfileProperties' {zendesk} -> zendesk) (\s@ConnectorProfileProperties' {} a -> s {zendesk = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Slack.
connectorProfileProperties_slack :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SlackConnectorProfileProperties)
connectorProfileProperties_slack = Lens.lens (\ConnectorProfileProperties' {slack} -> slack) (\s@ConnectorProfileProperties' {} a -> s {slack = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Singular.
connectorProfileProperties_singular :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SingularConnectorProfileProperties)
connectorProfileProperties_singular = Lens.lens (\ConnectorProfileProperties' {singular} -> singular) (\s@ConnectorProfileProperties' {} a -> s {singular = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Veeva.
connectorProfileProperties_veeva :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe VeevaConnectorProfileProperties)
connectorProfileProperties_veeva = Lens.lens (\ConnectorProfileProperties' {veeva} -> veeva) (\s@ConnectorProfileProperties' {} a -> s {veeva = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Amazon Honeycode.
connectorProfileProperties_honeycode :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe HoneycodeConnectorProfileProperties)
connectorProfileProperties_honeycode = Lens.lens (\ConnectorProfileProperties' {honeycode} -> honeycode) (\s@ConnectorProfileProperties' {} a -> s {honeycode = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Salesforce.
connectorProfileProperties_salesforce :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SalesforceConnectorProfileProperties)
connectorProfileProperties_salesforce = Lens.lens (\ConnectorProfileProperties' {salesforce} -> salesforce) (\s@ConnectorProfileProperties' {} a -> s {salesforce = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Snowflake.
connectorProfileProperties_snowflake :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SnowflakeConnectorProfileProperties)
connectorProfileProperties_snowflake = Lens.lens (\ConnectorProfileProperties' {snowflake} -> snowflake) (\s@ConnectorProfileProperties' {} a -> s {snowflake = a} :: ConnectorProfileProperties)

-- | Undocumented member.
connectorProfileProperties_sAPOData :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SAPODataConnectorProfileProperties)
connectorProfileProperties_sAPOData = Lens.lens (\ConnectorProfileProperties' {sAPOData} -> sAPOData) (\s@ConnectorProfileProperties' {} a -> s {sAPOData = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Marketo.
connectorProfileProperties_marketo :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe MarketoConnectorProfileProperties)
connectorProfileProperties_marketo = Lens.lens (\ConnectorProfileProperties' {marketo} -> marketo) (\s@ConnectorProfileProperties' {} a -> s {marketo = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Amazon Redshift.
connectorProfileProperties_redshift :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe RedshiftConnectorProfileProperties)
connectorProfileProperties_redshift = Lens.lens (\ConnectorProfileProperties' {redshift} -> redshift) (\s@ConnectorProfileProperties' {} a -> s {redshift = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Trend Micro.
connectorProfileProperties_trendmicro :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe TrendmicroConnectorProfileProperties)
connectorProfileProperties_trendmicro = Lens.lens (\ConnectorProfileProperties' {trendmicro} -> trendmicro) (\s@ConnectorProfileProperties' {} a -> s {trendmicro = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Infor Nexus.
connectorProfileProperties_inforNexus :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe InforNexusConnectorProfileProperties)
connectorProfileProperties_inforNexus = Lens.lens (\ConnectorProfileProperties' {inforNexus} -> inforNexus) (\s@ConnectorProfileProperties' {} a -> s {inforNexus = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by serviceNow.
connectorProfileProperties_serviceNow :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe ServiceNowConnectorProfileProperties)
connectorProfileProperties_serviceNow = Lens.lens (\ConnectorProfileProperties' {serviceNow} -> serviceNow) (\s@ConnectorProfileProperties' {} a -> s {serviceNow = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Datadog.
connectorProfileProperties_datadog :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe DatadogConnectorProfileProperties)
connectorProfileProperties_datadog = Lens.lens (\ConnectorProfileProperties' {datadog} -> datadog) (\s@ConnectorProfileProperties' {} a -> s {datadog = a} :: ConnectorProfileProperties)

-- | The properties required by the custom connector.
connectorProfileProperties_customConnector :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe CustomConnectorProfileProperties)
connectorProfileProperties_customConnector = Lens.lens (\ConnectorProfileProperties' {customConnector} -> customConnector) (\s@ConnectorProfileProperties' {} a -> s {customConnector = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Amplitude.
connectorProfileProperties_amplitude :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe AmplitudeConnectorProfileProperties)
connectorProfileProperties_amplitude = Lens.lens (\ConnectorProfileProperties' {amplitude} -> amplitude) (\s@ConnectorProfileProperties' {} a -> s {amplitude = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Dynatrace.
connectorProfileProperties_dynatrace :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe DynatraceConnectorProfileProperties)
connectorProfileProperties_dynatrace = Lens.lens (\ConnectorProfileProperties' {dynatrace} -> dynatrace) (\s@ConnectorProfileProperties' {} a -> s {dynatrace = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required Google Analytics.
connectorProfileProperties_googleAnalytics :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe GoogleAnalyticsConnectorProfileProperties)
connectorProfileProperties_googleAnalytics = Lens.lens (\ConnectorProfileProperties' {googleAnalytics} -> googleAnalytics) (\s@ConnectorProfileProperties' {} a -> s {googleAnalytics = a} :: ConnectorProfileProperties)

instance Core.FromJSON ConnectorProfileProperties where
  parseJSON =
    Core.withObject
      "ConnectorProfileProperties"
      ( \x ->
          ConnectorProfileProperties'
            Prelude.<$> (x Core..:? "Zendesk")
            Prelude.<*> (x Core..:? "Slack")
            Prelude.<*> (x Core..:? "Singular")
            Prelude.<*> (x Core..:? "Veeva")
            Prelude.<*> (x Core..:? "Honeycode")
            Prelude.<*> (x Core..:? "Salesforce")
            Prelude.<*> (x Core..:? "Snowflake")
            Prelude.<*> (x Core..:? "SAPOData")
            Prelude.<*> (x Core..:? "Marketo")
            Prelude.<*> (x Core..:? "Redshift")
            Prelude.<*> (x Core..:? "Trendmicro")
            Prelude.<*> (x Core..:? "InforNexus")
            Prelude.<*> (x Core..:? "ServiceNow")
            Prelude.<*> (x Core..:? "Datadog")
            Prelude.<*> (x Core..:? "CustomConnector")
            Prelude.<*> (x Core..:? "Amplitude")
            Prelude.<*> (x Core..:? "Dynatrace")
            Prelude.<*> (x Core..:? "GoogleAnalytics")
      )

instance Prelude.Hashable ConnectorProfileProperties where
  hashWithSalt _salt ConnectorProfileProperties' {..} =
    _salt `Prelude.hashWithSalt` zendesk
      `Prelude.hashWithSalt` slack
      `Prelude.hashWithSalt` singular
      `Prelude.hashWithSalt` veeva
      `Prelude.hashWithSalt` honeycode
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` snowflake
      `Prelude.hashWithSalt` sAPOData
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` redshift
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` inforNexus
      `Prelude.hashWithSalt` serviceNow
      `Prelude.hashWithSalt` datadog
      `Prelude.hashWithSalt` customConnector
      `Prelude.hashWithSalt` amplitude
      `Prelude.hashWithSalt` dynatrace
      `Prelude.hashWithSalt` googleAnalytics

instance Prelude.NFData ConnectorProfileProperties where
  rnf ConnectorProfileProperties' {..} =
    Prelude.rnf zendesk
      `Prelude.seq` Prelude.rnf slack
      `Prelude.seq` Prelude.rnf singular
      `Prelude.seq` Prelude.rnf veeva
      `Prelude.seq` Prelude.rnf honeycode
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf snowflake
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf redshift
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf inforNexus
      `Prelude.seq` Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf datadog
      `Prelude.seq` Prelude.rnf customConnector
      `Prelude.seq` Prelude.rnf amplitude
      `Prelude.seq` Prelude.rnf dynatrace
      `Prelude.seq` Prelude.rnf googleAnalytics

instance Core.ToJSON ConnectorProfileProperties where
  toJSON ConnectorProfileProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Zendesk" Core..=) Prelude.<$> zendesk,
            ("Slack" Core..=) Prelude.<$> slack,
            ("Singular" Core..=) Prelude.<$> singular,
            ("Veeva" Core..=) Prelude.<$> veeva,
            ("Honeycode" Core..=) Prelude.<$> honeycode,
            ("Salesforce" Core..=) Prelude.<$> salesforce,
            ("Snowflake" Core..=) Prelude.<$> snowflake,
            ("SAPOData" Core..=) Prelude.<$> sAPOData,
            ("Marketo" Core..=) Prelude.<$> marketo,
            ("Redshift" Core..=) Prelude.<$> redshift,
            ("Trendmicro" Core..=) Prelude.<$> trendmicro,
            ("InforNexus" Core..=) Prelude.<$> inforNexus,
            ("ServiceNow" Core..=) Prelude.<$> serviceNow,
            ("Datadog" Core..=) Prelude.<$> datadog,
            ("CustomConnector" Core..=)
              Prelude.<$> customConnector,
            ("Amplitude" Core..=) Prelude.<$> amplitude,
            ("Dynatrace" Core..=) Prelude.<$> dynatrace,
            ("GoogleAnalytics" Core..=)
              Prelude.<$> googleAnalytics
          ]
      )
