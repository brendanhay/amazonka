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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required by each connector.
--
-- /See:/ 'newConnectorProfileProperties' smart constructor.
data ConnectorProfileProperties = ConnectorProfileProperties'
  { -- | The connector-specific properties required by Amplitude.
    amplitude :: Prelude.Maybe AmplitudeConnectorProfileProperties,
    -- | The properties required by the custom connector.
    customConnector :: Prelude.Maybe CustomConnectorProfileProperties,
    -- | The connector-specific properties required by Datadog.
    datadog :: Prelude.Maybe DatadogConnectorProfileProperties,
    -- | The connector-specific properties required by Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceConnectorProfileProperties,
    -- | The connector-specific properties required Google Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsConnectorProfileProperties,
    -- | The connector-specific properties required by Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeConnectorProfileProperties,
    -- | The connector-specific properties required by Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusConnectorProfileProperties,
    -- | The connector-specific properties required by Marketo.
    marketo :: Prelude.Maybe MarketoConnectorProfileProperties,
    -- | The connector-specific properties required by Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftConnectorProfileProperties,
    sAPOData :: Prelude.Maybe SAPODataConnectorProfileProperties,
    -- | The connector-specific properties required by Salesforce.
    salesforce :: Prelude.Maybe SalesforceConnectorProfileProperties,
    -- | The connector-specific properties required by serviceNow.
    serviceNow :: Prelude.Maybe ServiceNowConnectorProfileProperties,
    -- | The connector-specific properties required by Singular.
    singular :: Prelude.Maybe SingularConnectorProfileProperties,
    -- | The connector-specific properties required by Slack.
    slack :: Prelude.Maybe SlackConnectorProfileProperties,
    -- | The connector-specific properties required by Snowflake.
    snowflake :: Prelude.Maybe SnowflakeConnectorProfileProperties,
    -- | The connector-specific properties required by Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroConnectorProfileProperties,
    -- | The connector-specific properties required by Veeva.
    veeva :: Prelude.Maybe VeevaConnectorProfileProperties,
    -- | The connector-specific properties required by Zendesk.
    zendesk :: Prelude.Maybe ZendeskConnectorProfileProperties
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
-- 'amplitude', 'connectorProfileProperties_amplitude' - The connector-specific properties required by Amplitude.
--
-- 'customConnector', 'connectorProfileProperties_customConnector' - The properties required by the custom connector.
--
-- 'datadog', 'connectorProfileProperties_datadog' - The connector-specific properties required by Datadog.
--
-- 'dynatrace', 'connectorProfileProperties_dynatrace' - The connector-specific properties required by Dynatrace.
--
-- 'googleAnalytics', 'connectorProfileProperties_googleAnalytics' - The connector-specific properties required Google Analytics.
--
-- 'honeycode', 'connectorProfileProperties_honeycode' - The connector-specific properties required by Amazon Honeycode.
--
-- 'inforNexus', 'connectorProfileProperties_inforNexus' - The connector-specific properties required by Infor Nexus.
--
-- 'marketo', 'connectorProfileProperties_marketo' - The connector-specific properties required by Marketo.
--
-- 'redshift', 'connectorProfileProperties_redshift' - The connector-specific properties required by Amazon Redshift.
--
-- 'sAPOData', 'connectorProfileProperties_sAPOData' - Undocumented member.
--
-- 'salesforce', 'connectorProfileProperties_salesforce' - The connector-specific properties required by Salesforce.
--
-- 'serviceNow', 'connectorProfileProperties_serviceNow' - The connector-specific properties required by serviceNow.
--
-- 'singular', 'connectorProfileProperties_singular' - The connector-specific properties required by Singular.
--
-- 'slack', 'connectorProfileProperties_slack' - The connector-specific properties required by Slack.
--
-- 'snowflake', 'connectorProfileProperties_snowflake' - The connector-specific properties required by Snowflake.
--
-- 'trendmicro', 'connectorProfileProperties_trendmicro' - The connector-specific properties required by Trend Micro.
--
-- 'veeva', 'connectorProfileProperties_veeva' - The connector-specific properties required by Veeva.
--
-- 'zendesk', 'connectorProfileProperties_zendesk' - The connector-specific properties required by Zendesk.
newConnectorProfileProperties ::
  ConnectorProfileProperties
newConnectorProfileProperties =
  ConnectorProfileProperties'
    { amplitude =
        Prelude.Nothing,
      customConnector = Prelude.Nothing,
      datadog = Prelude.Nothing,
      dynatrace = Prelude.Nothing,
      googleAnalytics = Prelude.Nothing,
      honeycode = Prelude.Nothing,
      inforNexus = Prelude.Nothing,
      marketo = Prelude.Nothing,
      redshift = Prelude.Nothing,
      sAPOData = Prelude.Nothing,
      salesforce = Prelude.Nothing,
      serviceNow = Prelude.Nothing,
      singular = Prelude.Nothing,
      slack = Prelude.Nothing,
      snowflake = Prelude.Nothing,
      trendmicro = Prelude.Nothing,
      veeva = Prelude.Nothing,
      zendesk = Prelude.Nothing
    }

-- | The connector-specific properties required by Amplitude.
connectorProfileProperties_amplitude :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe AmplitudeConnectorProfileProperties)
connectorProfileProperties_amplitude = Lens.lens (\ConnectorProfileProperties' {amplitude} -> amplitude) (\s@ConnectorProfileProperties' {} a -> s {amplitude = a} :: ConnectorProfileProperties)

-- | The properties required by the custom connector.
connectorProfileProperties_customConnector :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe CustomConnectorProfileProperties)
connectorProfileProperties_customConnector = Lens.lens (\ConnectorProfileProperties' {customConnector} -> customConnector) (\s@ConnectorProfileProperties' {} a -> s {customConnector = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Datadog.
connectorProfileProperties_datadog :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe DatadogConnectorProfileProperties)
connectorProfileProperties_datadog = Lens.lens (\ConnectorProfileProperties' {datadog} -> datadog) (\s@ConnectorProfileProperties' {} a -> s {datadog = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Dynatrace.
connectorProfileProperties_dynatrace :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe DynatraceConnectorProfileProperties)
connectorProfileProperties_dynatrace = Lens.lens (\ConnectorProfileProperties' {dynatrace} -> dynatrace) (\s@ConnectorProfileProperties' {} a -> s {dynatrace = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required Google Analytics.
connectorProfileProperties_googleAnalytics :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe GoogleAnalyticsConnectorProfileProperties)
connectorProfileProperties_googleAnalytics = Lens.lens (\ConnectorProfileProperties' {googleAnalytics} -> googleAnalytics) (\s@ConnectorProfileProperties' {} a -> s {googleAnalytics = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Amazon Honeycode.
connectorProfileProperties_honeycode :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe HoneycodeConnectorProfileProperties)
connectorProfileProperties_honeycode = Lens.lens (\ConnectorProfileProperties' {honeycode} -> honeycode) (\s@ConnectorProfileProperties' {} a -> s {honeycode = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Infor Nexus.
connectorProfileProperties_inforNexus :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe InforNexusConnectorProfileProperties)
connectorProfileProperties_inforNexus = Lens.lens (\ConnectorProfileProperties' {inforNexus} -> inforNexus) (\s@ConnectorProfileProperties' {} a -> s {inforNexus = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Marketo.
connectorProfileProperties_marketo :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe MarketoConnectorProfileProperties)
connectorProfileProperties_marketo = Lens.lens (\ConnectorProfileProperties' {marketo} -> marketo) (\s@ConnectorProfileProperties' {} a -> s {marketo = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Amazon Redshift.
connectorProfileProperties_redshift :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe RedshiftConnectorProfileProperties)
connectorProfileProperties_redshift = Lens.lens (\ConnectorProfileProperties' {redshift} -> redshift) (\s@ConnectorProfileProperties' {} a -> s {redshift = a} :: ConnectorProfileProperties)

-- | Undocumented member.
connectorProfileProperties_sAPOData :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SAPODataConnectorProfileProperties)
connectorProfileProperties_sAPOData = Lens.lens (\ConnectorProfileProperties' {sAPOData} -> sAPOData) (\s@ConnectorProfileProperties' {} a -> s {sAPOData = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Salesforce.
connectorProfileProperties_salesforce :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SalesforceConnectorProfileProperties)
connectorProfileProperties_salesforce = Lens.lens (\ConnectorProfileProperties' {salesforce} -> salesforce) (\s@ConnectorProfileProperties' {} a -> s {salesforce = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by serviceNow.
connectorProfileProperties_serviceNow :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe ServiceNowConnectorProfileProperties)
connectorProfileProperties_serviceNow = Lens.lens (\ConnectorProfileProperties' {serviceNow} -> serviceNow) (\s@ConnectorProfileProperties' {} a -> s {serviceNow = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Singular.
connectorProfileProperties_singular :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SingularConnectorProfileProperties)
connectorProfileProperties_singular = Lens.lens (\ConnectorProfileProperties' {singular} -> singular) (\s@ConnectorProfileProperties' {} a -> s {singular = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Slack.
connectorProfileProperties_slack :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SlackConnectorProfileProperties)
connectorProfileProperties_slack = Lens.lens (\ConnectorProfileProperties' {slack} -> slack) (\s@ConnectorProfileProperties' {} a -> s {slack = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Snowflake.
connectorProfileProperties_snowflake :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe SnowflakeConnectorProfileProperties)
connectorProfileProperties_snowflake = Lens.lens (\ConnectorProfileProperties' {snowflake} -> snowflake) (\s@ConnectorProfileProperties' {} a -> s {snowflake = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Trend Micro.
connectorProfileProperties_trendmicro :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe TrendmicroConnectorProfileProperties)
connectorProfileProperties_trendmicro = Lens.lens (\ConnectorProfileProperties' {trendmicro} -> trendmicro) (\s@ConnectorProfileProperties' {} a -> s {trendmicro = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Veeva.
connectorProfileProperties_veeva :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe VeevaConnectorProfileProperties)
connectorProfileProperties_veeva = Lens.lens (\ConnectorProfileProperties' {veeva} -> veeva) (\s@ConnectorProfileProperties' {} a -> s {veeva = a} :: ConnectorProfileProperties)

-- | The connector-specific properties required by Zendesk.
connectorProfileProperties_zendesk :: Lens.Lens' ConnectorProfileProperties (Prelude.Maybe ZendeskConnectorProfileProperties)
connectorProfileProperties_zendesk = Lens.lens (\ConnectorProfileProperties' {zendesk} -> zendesk) (\s@ConnectorProfileProperties' {} a -> s {zendesk = a} :: ConnectorProfileProperties)

instance Data.FromJSON ConnectorProfileProperties where
  parseJSON =
    Data.withObject
      "ConnectorProfileProperties"
      ( \x ->
          ConnectorProfileProperties'
            Prelude.<$> (x Data..:? "Amplitude")
            Prelude.<*> (x Data..:? "CustomConnector")
            Prelude.<*> (x Data..:? "Datadog")
            Prelude.<*> (x Data..:? "Dynatrace")
            Prelude.<*> (x Data..:? "GoogleAnalytics")
            Prelude.<*> (x Data..:? "Honeycode")
            Prelude.<*> (x Data..:? "InforNexus")
            Prelude.<*> (x Data..:? "Marketo")
            Prelude.<*> (x Data..:? "Redshift")
            Prelude.<*> (x Data..:? "SAPOData")
            Prelude.<*> (x Data..:? "Salesforce")
            Prelude.<*> (x Data..:? "ServiceNow")
            Prelude.<*> (x Data..:? "Singular")
            Prelude.<*> (x Data..:? "Slack")
            Prelude.<*> (x Data..:? "Snowflake")
            Prelude.<*> (x Data..:? "Trendmicro")
            Prelude.<*> (x Data..:? "Veeva")
            Prelude.<*> (x Data..:? "Zendesk")
      )

instance Prelude.Hashable ConnectorProfileProperties where
  hashWithSalt _salt ConnectorProfileProperties' {..} =
    _salt
      `Prelude.hashWithSalt` amplitude
      `Prelude.hashWithSalt` customConnector
      `Prelude.hashWithSalt` datadog
      `Prelude.hashWithSalt` dynatrace
      `Prelude.hashWithSalt` googleAnalytics
      `Prelude.hashWithSalt` honeycode
      `Prelude.hashWithSalt` inforNexus
      `Prelude.hashWithSalt` marketo
      `Prelude.hashWithSalt` redshift
      `Prelude.hashWithSalt` sAPOData
      `Prelude.hashWithSalt` salesforce
      `Prelude.hashWithSalt` serviceNow
      `Prelude.hashWithSalt` singular
      `Prelude.hashWithSalt` slack
      `Prelude.hashWithSalt` snowflake
      `Prelude.hashWithSalt` trendmicro
      `Prelude.hashWithSalt` veeva
      `Prelude.hashWithSalt` zendesk

instance Prelude.NFData ConnectorProfileProperties where
  rnf ConnectorProfileProperties' {..} =
    Prelude.rnf amplitude
      `Prelude.seq` Prelude.rnf customConnector
      `Prelude.seq` Prelude.rnf datadog
      `Prelude.seq` Prelude.rnf dynatrace
      `Prelude.seq` Prelude.rnf googleAnalytics
      `Prelude.seq` Prelude.rnf honeycode
      `Prelude.seq` Prelude.rnf inforNexus
      `Prelude.seq` Prelude.rnf marketo
      `Prelude.seq` Prelude.rnf redshift
      `Prelude.seq` Prelude.rnf sAPOData
      `Prelude.seq` Prelude.rnf salesforce
      `Prelude.seq` Prelude.rnf serviceNow
      `Prelude.seq` Prelude.rnf singular
      `Prelude.seq` Prelude.rnf slack
      `Prelude.seq` Prelude.rnf snowflake
      `Prelude.seq` Prelude.rnf trendmicro
      `Prelude.seq` Prelude.rnf veeva
      `Prelude.seq` Prelude.rnf zendesk

instance Data.ToJSON ConnectorProfileProperties where
  toJSON ConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Amplitude" Data..=) Prelude.<$> amplitude,
            ("CustomConnector" Data..=)
              Prelude.<$> customConnector,
            ("Datadog" Data..=) Prelude.<$> datadog,
            ("Dynatrace" Data..=) Prelude.<$> dynatrace,
            ("GoogleAnalytics" Data..=)
              Prelude.<$> googleAnalytics,
            ("Honeycode" Data..=) Prelude.<$> honeycode,
            ("InforNexus" Data..=) Prelude.<$> inforNexus,
            ("Marketo" Data..=) Prelude.<$> marketo,
            ("Redshift" Data..=) Prelude.<$> redshift,
            ("SAPOData" Data..=) Prelude.<$> sAPOData,
            ("Salesforce" Data..=) Prelude.<$> salesforce,
            ("ServiceNow" Data..=) Prelude.<$> serviceNow,
            ("Singular" Data..=) Prelude.<$> singular,
            ("Slack" Data..=) Prelude.<$> slack,
            ("Snowflake" Data..=) Prelude.<$> snowflake,
            ("Trendmicro" Data..=) Prelude.<$> trendmicro,
            ("Veeva" Data..=) Prelude.<$> veeva,
            ("Zendesk" Data..=) Prelude.<$> zendesk
          ]
      )
