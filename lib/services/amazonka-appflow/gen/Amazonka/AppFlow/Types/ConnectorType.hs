{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppFlow.Types.ConnectorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ConnectorType
  ( ConnectorType
      ( ..,
        ConnectorType_Amplitude,
        ConnectorType_CustomConnector,
        ConnectorType_CustomerProfiles,
        ConnectorType_Datadog,
        ConnectorType_Dynatrace,
        ConnectorType_EventBridge,
        ConnectorType_Googleanalytics,
        ConnectorType_Honeycode,
        ConnectorType_Infornexus,
        ConnectorType_LookoutMetrics,
        ConnectorType_Marketo,
        ConnectorType_Redshift,
        ConnectorType_S3,
        ConnectorType_SAPOData,
        ConnectorType_Salesforce,
        ConnectorType_Servicenow,
        ConnectorType_Singular,
        ConnectorType_Slack,
        ConnectorType_Snowflake,
        ConnectorType_Trendmicro,
        ConnectorType_Upsolver,
        ConnectorType_Veeva,
        ConnectorType_Zendesk
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConnectorType = ConnectorType'
  { fromConnectorType ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ConnectorType_Amplitude :: ConnectorType
pattern ConnectorType_Amplitude = ConnectorType' "Amplitude"

pattern ConnectorType_CustomConnector :: ConnectorType
pattern ConnectorType_CustomConnector = ConnectorType' "CustomConnector"

pattern ConnectorType_CustomerProfiles :: ConnectorType
pattern ConnectorType_CustomerProfiles = ConnectorType' "CustomerProfiles"

pattern ConnectorType_Datadog :: ConnectorType
pattern ConnectorType_Datadog = ConnectorType' "Datadog"

pattern ConnectorType_Dynatrace :: ConnectorType
pattern ConnectorType_Dynatrace = ConnectorType' "Dynatrace"

pattern ConnectorType_EventBridge :: ConnectorType
pattern ConnectorType_EventBridge = ConnectorType' "EventBridge"

pattern ConnectorType_Googleanalytics :: ConnectorType
pattern ConnectorType_Googleanalytics = ConnectorType' "Googleanalytics"

pattern ConnectorType_Honeycode :: ConnectorType
pattern ConnectorType_Honeycode = ConnectorType' "Honeycode"

pattern ConnectorType_Infornexus :: ConnectorType
pattern ConnectorType_Infornexus = ConnectorType' "Infornexus"

pattern ConnectorType_LookoutMetrics :: ConnectorType
pattern ConnectorType_LookoutMetrics = ConnectorType' "LookoutMetrics"

pattern ConnectorType_Marketo :: ConnectorType
pattern ConnectorType_Marketo = ConnectorType' "Marketo"

pattern ConnectorType_Redshift :: ConnectorType
pattern ConnectorType_Redshift = ConnectorType' "Redshift"

pattern ConnectorType_S3 :: ConnectorType
pattern ConnectorType_S3 = ConnectorType' "S3"

pattern ConnectorType_SAPOData :: ConnectorType
pattern ConnectorType_SAPOData = ConnectorType' "SAPOData"

pattern ConnectorType_Salesforce :: ConnectorType
pattern ConnectorType_Salesforce = ConnectorType' "Salesforce"

pattern ConnectorType_Servicenow :: ConnectorType
pattern ConnectorType_Servicenow = ConnectorType' "Servicenow"

pattern ConnectorType_Singular :: ConnectorType
pattern ConnectorType_Singular = ConnectorType' "Singular"

pattern ConnectorType_Slack :: ConnectorType
pattern ConnectorType_Slack = ConnectorType' "Slack"

pattern ConnectorType_Snowflake :: ConnectorType
pattern ConnectorType_Snowflake = ConnectorType' "Snowflake"

pattern ConnectorType_Trendmicro :: ConnectorType
pattern ConnectorType_Trendmicro = ConnectorType' "Trendmicro"

pattern ConnectorType_Upsolver :: ConnectorType
pattern ConnectorType_Upsolver = ConnectorType' "Upsolver"

pattern ConnectorType_Veeva :: ConnectorType
pattern ConnectorType_Veeva = ConnectorType' "Veeva"

pattern ConnectorType_Zendesk :: ConnectorType
pattern ConnectorType_Zendesk = ConnectorType' "Zendesk"

{-# COMPLETE
  ConnectorType_Amplitude,
  ConnectorType_CustomConnector,
  ConnectorType_CustomerProfiles,
  ConnectorType_Datadog,
  ConnectorType_Dynatrace,
  ConnectorType_EventBridge,
  ConnectorType_Googleanalytics,
  ConnectorType_Honeycode,
  ConnectorType_Infornexus,
  ConnectorType_LookoutMetrics,
  ConnectorType_Marketo,
  ConnectorType_Redshift,
  ConnectorType_S3,
  ConnectorType_SAPOData,
  ConnectorType_Salesforce,
  ConnectorType_Servicenow,
  ConnectorType_Singular,
  ConnectorType_Slack,
  ConnectorType_Snowflake,
  ConnectorType_Trendmicro,
  ConnectorType_Upsolver,
  ConnectorType_Veeva,
  ConnectorType_Zendesk,
  ConnectorType'
  #-}
