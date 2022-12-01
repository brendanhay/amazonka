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
-- Module      : Amazonka.AppFlow.Types.ConnectorProfileCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorProfileCredentials where

import Amazonka.AppFlow.Types.AmplitudeConnectorProfileCredentials
import Amazonka.AppFlow.Types.CustomConnectorProfileCredentials
import Amazonka.AppFlow.Types.DatadogConnectorProfileCredentials
import Amazonka.AppFlow.Types.DynatraceConnectorProfileCredentials
import Amazonka.AppFlow.Types.GoogleAnalyticsConnectorProfileCredentials
import Amazonka.AppFlow.Types.HoneycodeConnectorProfileCredentials
import Amazonka.AppFlow.Types.InforNexusConnectorProfileCredentials
import Amazonka.AppFlow.Types.MarketoConnectorProfileCredentials
import Amazonka.AppFlow.Types.RedshiftConnectorProfileCredentials
import Amazonka.AppFlow.Types.SAPODataConnectorProfileCredentials
import Amazonka.AppFlow.Types.SalesforceConnectorProfileCredentials
import Amazonka.AppFlow.Types.ServiceNowConnectorProfileCredentials
import Amazonka.AppFlow.Types.SingularConnectorProfileCredentials
import Amazonka.AppFlow.Types.SlackConnectorProfileCredentials
import Amazonka.AppFlow.Types.SnowflakeConnectorProfileCredentials
import Amazonka.AppFlow.Types.TrendmicroConnectorProfileCredentials
import Amazonka.AppFlow.Types.VeevaConnectorProfileCredentials
import Amazonka.AppFlow.Types.ZendeskConnectorProfileCredentials
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific credentials required by a connector.
--
-- /See:/ 'newConnectorProfileCredentials' smart constructor.
data ConnectorProfileCredentials = ConnectorProfileCredentials'
  { -- | The connector-specific credentials required when using Zendesk.
    zendesk :: Prelude.Maybe ZendeskConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Slack.
    slack :: Prelude.Maybe SlackConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Singular.
    singular :: Prelude.Maybe SingularConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Veeva.
    veeva :: Prelude.Maybe VeevaConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Amazon Honeycode.
    honeycode :: Prelude.Maybe HoneycodeConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Salesforce.
    salesforce :: Prelude.Maybe SalesforceConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Snowflake.
    snowflake :: Prelude.Maybe SnowflakeConnectorProfileCredentials,
    sAPOData :: Prelude.Maybe SAPODataConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Marketo.
    marketo :: Prelude.Maybe MarketoConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Amazon Redshift.
    redshift :: Prelude.Maybe RedshiftConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Trend Micro.
    trendmicro :: Prelude.Maybe TrendmicroConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Infor Nexus.
    inforNexus :: Prelude.Maybe InforNexusConnectorProfileCredentials,
    -- | The connector-specific credentials required when using ServiceNow.
    serviceNow :: Prelude.Maybe ServiceNowConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Datadog.
    datadog :: Prelude.Maybe DatadogConnectorProfileCredentials,
    customConnector :: Prelude.Maybe CustomConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Amplitude.
    amplitude :: Prelude.Maybe AmplitudeConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Dynatrace.
    dynatrace :: Prelude.Maybe DynatraceConnectorProfileCredentials,
    -- | The connector-specific credentials required when using Google Analytics.
    googleAnalytics :: Prelude.Maybe GoogleAnalyticsConnectorProfileCredentials
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zendesk', 'connectorProfileCredentials_zendesk' - The connector-specific credentials required when using Zendesk.
--
-- 'slack', 'connectorProfileCredentials_slack' - The connector-specific credentials required when using Slack.
--
-- 'singular', 'connectorProfileCredentials_singular' - The connector-specific credentials required when using Singular.
--
-- 'veeva', 'connectorProfileCredentials_veeva' - The connector-specific credentials required when using Veeva.
--
-- 'honeycode', 'connectorProfileCredentials_honeycode' - The connector-specific credentials required when using Amazon Honeycode.
--
-- 'salesforce', 'connectorProfileCredentials_salesforce' - The connector-specific credentials required when using Salesforce.
--
-- 'snowflake', 'connectorProfileCredentials_snowflake' - The connector-specific credentials required when using Snowflake.
--
-- 'sAPOData', 'connectorProfileCredentials_sAPOData' - Undocumented member.
--
-- 'marketo', 'connectorProfileCredentials_marketo' - The connector-specific credentials required when using Marketo.
--
-- 'redshift', 'connectorProfileCredentials_redshift' - The connector-specific credentials required when using Amazon Redshift.
--
-- 'trendmicro', 'connectorProfileCredentials_trendmicro' - The connector-specific credentials required when using Trend Micro.
--
-- 'inforNexus', 'connectorProfileCredentials_inforNexus' - The connector-specific credentials required when using Infor Nexus.
--
-- 'serviceNow', 'connectorProfileCredentials_serviceNow' - The connector-specific credentials required when using ServiceNow.
--
-- 'datadog', 'connectorProfileCredentials_datadog' - The connector-specific credentials required when using Datadog.
--
-- 'customConnector', 'connectorProfileCredentials_customConnector' - Undocumented member.
--
-- 'amplitude', 'connectorProfileCredentials_amplitude' - The connector-specific credentials required when using Amplitude.
--
-- 'dynatrace', 'connectorProfileCredentials_dynatrace' - The connector-specific credentials required when using Dynatrace.
--
-- 'googleAnalytics', 'connectorProfileCredentials_googleAnalytics' - The connector-specific credentials required when using Google Analytics.
newConnectorProfileCredentials ::
  ConnectorProfileCredentials
newConnectorProfileCredentials =
  ConnectorProfileCredentials'
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

-- | The connector-specific credentials required when using Zendesk.
connectorProfileCredentials_zendesk :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe ZendeskConnectorProfileCredentials)
connectorProfileCredentials_zendesk = Lens.lens (\ConnectorProfileCredentials' {zendesk} -> zendesk) (\s@ConnectorProfileCredentials' {} a -> s {zendesk = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Slack.
connectorProfileCredentials_slack :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SlackConnectorProfileCredentials)
connectorProfileCredentials_slack = Lens.lens (\ConnectorProfileCredentials' {slack} -> slack) (\s@ConnectorProfileCredentials' {} a -> s {slack = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Singular.
connectorProfileCredentials_singular :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SingularConnectorProfileCredentials)
connectorProfileCredentials_singular = Lens.lens (\ConnectorProfileCredentials' {singular} -> singular) (\s@ConnectorProfileCredentials' {} a -> s {singular = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Veeva.
connectorProfileCredentials_veeva :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe VeevaConnectorProfileCredentials)
connectorProfileCredentials_veeva = Lens.lens (\ConnectorProfileCredentials' {veeva} -> veeva) (\s@ConnectorProfileCredentials' {} a -> s {veeva = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Amazon Honeycode.
connectorProfileCredentials_honeycode :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe HoneycodeConnectorProfileCredentials)
connectorProfileCredentials_honeycode = Lens.lens (\ConnectorProfileCredentials' {honeycode} -> honeycode) (\s@ConnectorProfileCredentials' {} a -> s {honeycode = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Salesforce.
connectorProfileCredentials_salesforce :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SalesforceConnectorProfileCredentials)
connectorProfileCredentials_salesforce = Lens.lens (\ConnectorProfileCredentials' {salesforce} -> salesforce) (\s@ConnectorProfileCredentials' {} a -> s {salesforce = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Snowflake.
connectorProfileCredentials_snowflake :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SnowflakeConnectorProfileCredentials)
connectorProfileCredentials_snowflake = Lens.lens (\ConnectorProfileCredentials' {snowflake} -> snowflake) (\s@ConnectorProfileCredentials' {} a -> s {snowflake = a} :: ConnectorProfileCredentials)

-- | Undocumented member.
connectorProfileCredentials_sAPOData :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe SAPODataConnectorProfileCredentials)
connectorProfileCredentials_sAPOData = Lens.lens (\ConnectorProfileCredentials' {sAPOData} -> sAPOData) (\s@ConnectorProfileCredentials' {} a -> s {sAPOData = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Marketo.
connectorProfileCredentials_marketo :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe MarketoConnectorProfileCredentials)
connectorProfileCredentials_marketo = Lens.lens (\ConnectorProfileCredentials' {marketo} -> marketo) (\s@ConnectorProfileCredentials' {} a -> s {marketo = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Amazon Redshift.
connectorProfileCredentials_redshift :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe RedshiftConnectorProfileCredentials)
connectorProfileCredentials_redshift = Lens.lens (\ConnectorProfileCredentials' {redshift} -> redshift) (\s@ConnectorProfileCredentials' {} a -> s {redshift = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Trend Micro.
connectorProfileCredentials_trendmicro :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe TrendmicroConnectorProfileCredentials)
connectorProfileCredentials_trendmicro = Lens.lens (\ConnectorProfileCredentials' {trendmicro} -> trendmicro) (\s@ConnectorProfileCredentials' {} a -> s {trendmicro = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Infor Nexus.
connectorProfileCredentials_inforNexus :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe InforNexusConnectorProfileCredentials)
connectorProfileCredentials_inforNexus = Lens.lens (\ConnectorProfileCredentials' {inforNexus} -> inforNexus) (\s@ConnectorProfileCredentials' {} a -> s {inforNexus = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using ServiceNow.
connectorProfileCredentials_serviceNow :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe ServiceNowConnectorProfileCredentials)
connectorProfileCredentials_serviceNow = Lens.lens (\ConnectorProfileCredentials' {serviceNow} -> serviceNow) (\s@ConnectorProfileCredentials' {} a -> s {serviceNow = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Datadog.
connectorProfileCredentials_datadog :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe DatadogConnectorProfileCredentials)
connectorProfileCredentials_datadog = Lens.lens (\ConnectorProfileCredentials' {datadog} -> datadog) (\s@ConnectorProfileCredentials' {} a -> s {datadog = a} :: ConnectorProfileCredentials)

-- | Undocumented member.
connectorProfileCredentials_customConnector :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe CustomConnectorProfileCredentials)
connectorProfileCredentials_customConnector = Lens.lens (\ConnectorProfileCredentials' {customConnector} -> customConnector) (\s@ConnectorProfileCredentials' {} a -> s {customConnector = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Amplitude.
connectorProfileCredentials_amplitude :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe AmplitudeConnectorProfileCredentials)
connectorProfileCredentials_amplitude = Lens.lens (\ConnectorProfileCredentials' {amplitude} -> amplitude) (\s@ConnectorProfileCredentials' {} a -> s {amplitude = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Dynatrace.
connectorProfileCredentials_dynatrace :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe DynatraceConnectorProfileCredentials)
connectorProfileCredentials_dynatrace = Lens.lens (\ConnectorProfileCredentials' {dynatrace} -> dynatrace) (\s@ConnectorProfileCredentials' {} a -> s {dynatrace = a} :: ConnectorProfileCredentials)

-- | The connector-specific credentials required when using Google Analytics.
connectorProfileCredentials_googleAnalytics :: Lens.Lens' ConnectorProfileCredentials (Prelude.Maybe GoogleAnalyticsConnectorProfileCredentials)
connectorProfileCredentials_googleAnalytics = Lens.lens (\ConnectorProfileCredentials' {googleAnalytics} -> googleAnalytics) (\s@ConnectorProfileCredentials' {} a -> s {googleAnalytics = a} :: ConnectorProfileCredentials)

instance Prelude.Hashable ConnectorProfileCredentials where
  hashWithSalt _salt ConnectorProfileCredentials' {..} =
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

instance Prelude.NFData ConnectorProfileCredentials where
  rnf ConnectorProfileCredentials' {..} =
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

instance Core.ToJSON ConnectorProfileCredentials where
  toJSON ConnectorProfileCredentials' {..} =
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
