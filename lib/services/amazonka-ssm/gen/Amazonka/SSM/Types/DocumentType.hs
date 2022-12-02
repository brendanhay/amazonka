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
-- Module      : Amazonka.SSM.Types.DocumentType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.DocumentType
  ( DocumentType
      ( ..,
        DocumentType_ApplicationConfiguration,
        DocumentType_ApplicationConfigurationSchema,
        DocumentType_Automation,
        DocumentType_Automation_ChangeTemplate,
        DocumentType_ChangeCalendar,
        DocumentType_CloudFormation,
        DocumentType_Command,
        DocumentType_ConformancePackTemplate,
        DocumentType_DeploymentStrategy,
        DocumentType_Package,
        DocumentType_Policy,
        DocumentType_ProblemAnalysis,
        DocumentType_ProblemAnalysisTemplate,
        DocumentType_Session
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DocumentType = DocumentType'
  { fromDocumentType ::
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

pattern DocumentType_ApplicationConfiguration :: DocumentType
pattern DocumentType_ApplicationConfiguration = DocumentType' "ApplicationConfiguration"

pattern DocumentType_ApplicationConfigurationSchema :: DocumentType
pattern DocumentType_ApplicationConfigurationSchema = DocumentType' "ApplicationConfigurationSchema"

pattern DocumentType_Automation :: DocumentType
pattern DocumentType_Automation = DocumentType' "Automation"

pattern DocumentType_Automation_ChangeTemplate :: DocumentType
pattern DocumentType_Automation_ChangeTemplate = DocumentType' "Automation.ChangeTemplate"

pattern DocumentType_ChangeCalendar :: DocumentType
pattern DocumentType_ChangeCalendar = DocumentType' "ChangeCalendar"

pattern DocumentType_CloudFormation :: DocumentType
pattern DocumentType_CloudFormation = DocumentType' "CloudFormation"

pattern DocumentType_Command :: DocumentType
pattern DocumentType_Command = DocumentType' "Command"

pattern DocumentType_ConformancePackTemplate :: DocumentType
pattern DocumentType_ConformancePackTemplate = DocumentType' "ConformancePackTemplate"

pattern DocumentType_DeploymentStrategy :: DocumentType
pattern DocumentType_DeploymentStrategy = DocumentType' "DeploymentStrategy"

pattern DocumentType_Package :: DocumentType
pattern DocumentType_Package = DocumentType' "Package"

pattern DocumentType_Policy :: DocumentType
pattern DocumentType_Policy = DocumentType' "Policy"

pattern DocumentType_ProblemAnalysis :: DocumentType
pattern DocumentType_ProblemAnalysis = DocumentType' "ProblemAnalysis"

pattern DocumentType_ProblemAnalysisTemplate :: DocumentType
pattern DocumentType_ProblemAnalysisTemplate = DocumentType' "ProblemAnalysisTemplate"

pattern DocumentType_Session :: DocumentType
pattern DocumentType_Session = DocumentType' "Session"

{-# COMPLETE
  DocumentType_ApplicationConfiguration,
  DocumentType_ApplicationConfigurationSchema,
  DocumentType_Automation,
  DocumentType_Automation_ChangeTemplate,
  DocumentType_ChangeCalendar,
  DocumentType_CloudFormation,
  DocumentType_Command,
  DocumentType_ConformancePackTemplate,
  DocumentType_DeploymentStrategy,
  DocumentType_Package,
  DocumentType_Policy,
  DocumentType_ProblemAnalysis,
  DocumentType_ProblemAnalysisTemplate,
  DocumentType_Session,
  DocumentType'
  #-}
