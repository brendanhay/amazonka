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
-- Module      : Amazonka.AuditManager.Types.SettingAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.SettingAttribute
  ( SettingAttribute
      ( ..,
        SettingAttribute_ALL,
        SettingAttribute_DEFAULT_ASSESSMENT_REPORTS_DESTINATION,
        SettingAttribute_DEFAULT_EXPORT_DESTINATION,
        SettingAttribute_DEFAULT_PROCESS_OWNERS,
        SettingAttribute_DEREGISTRATION_POLICY,
        SettingAttribute_EVIDENCE_FINDER_ENABLEMENT,
        SettingAttribute_IS_AWS_ORG_ENABLED,
        SettingAttribute_SNS_TOPIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SettingAttribute = SettingAttribute'
  { fromSettingAttribute ::
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

pattern SettingAttribute_ALL :: SettingAttribute
pattern SettingAttribute_ALL = SettingAttribute' "ALL"

pattern SettingAttribute_DEFAULT_ASSESSMENT_REPORTS_DESTINATION :: SettingAttribute
pattern SettingAttribute_DEFAULT_ASSESSMENT_REPORTS_DESTINATION = SettingAttribute' "DEFAULT_ASSESSMENT_REPORTS_DESTINATION"

pattern SettingAttribute_DEFAULT_EXPORT_DESTINATION :: SettingAttribute
pattern SettingAttribute_DEFAULT_EXPORT_DESTINATION = SettingAttribute' "DEFAULT_EXPORT_DESTINATION"

pattern SettingAttribute_DEFAULT_PROCESS_OWNERS :: SettingAttribute
pattern SettingAttribute_DEFAULT_PROCESS_OWNERS = SettingAttribute' "DEFAULT_PROCESS_OWNERS"

pattern SettingAttribute_DEREGISTRATION_POLICY :: SettingAttribute
pattern SettingAttribute_DEREGISTRATION_POLICY = SettingAttribute' "DEREGISTRATION_POLICY"

pattern SettingAttribute_EVIDENCE_FINDER_ENABLEMENT :: SettingAttribute
pattern SettingAttribute_EVIDENCE_FINDER_ENABLEMENT = SettingAttribute' "EVIDENCE_FINDER_ENABLEMENT"

pattern SettingAttribute_IS_AWS_ORG_ENABLED :: SettingAttribute
pattern SettingAttribute_IS_AWS_ORG_ENABLED = SettingAttribute' "IS_AWS_ORG_ENABLED"

pattern SettingAttribute_SNS_TOPIC :: SettingAttribute
pattern SettingAttribute_SNS_TOPIC = SettingAttribute' "SNS_TOPIC"

{-# COMPLETE
  SettingAttribute_ALL,
  SettingAttribute_DEFAULT_ASSESSMENT_REPORTS_DESTINATION,
  SettingAttribute_DEFAULT_EXPORT_DESTINATION,
  SettingAttribute_DEFAULT_PROCESS_OWNERS,
  SettingAttribute_DEREGISTRATION_POLICY,
  SettingAttribute_EVIDENCE_FINDER_ENABLEMENT,
  SettingAttribute_IS_AWS_ORG_ENABLED,
  SettingAttribute_SNS_TOPIC,
  SettingAttribute'
  #-}
