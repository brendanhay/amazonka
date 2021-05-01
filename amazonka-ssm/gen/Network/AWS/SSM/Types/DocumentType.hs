{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentType
  ( DocumentType
      ( ..,
        DocumentType_ApplicationConfiguration,
        DocumentType_ApplicationConfigurationSchema,
        DocumentType_Automation,
        DocumentType_Automation_ChangeTemplate,
        DocumentType_ChangeCalendar,
        DocumentType_Command,
        DocumentType_DeploymentStrategy,
        DocumentType_Package,
        DocumentType_Policy,
        DocumentType_Session
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DocumentType = DocumentType'
  { fromDocumentType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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

pattern DocumentType_Command :: DocumentType
pattern DocumentType_Command = DocumentType' "Command"

pattern DocumentType_DeploymentStrategy :: DocumentType
pattern DocumentType_DeploymentStrategy = DocumentType' "DeploymentStrategy"

pattern DocumentType_Package :: DocumentType
pattern DocumentType_Package = DocumentType' "Package"

pattern DocumentType_Policy :: DocumentType
pattern DocumentType_Policy = DocumentType' "Policy"

pattern DocumentType_Session :: DocumentType
pattern DocumentType_Session = DocumentType' "Session"

{-# COMPLETE
  DocumentType_ApplicationConfiguration,
  DocumentType_ApplicationConfigurationSchema,
  DocumentType_Automation,
  DocumentType_Automation_ChangeTemplate,
  DocumentType_ChangeCalendar,
  DocumentType_Command,
  DocumentType_DeploymentStrategy,
  DocumentType_Package,
  DocumentType_Policy,
  DocumentType_Session,
  DocumentType'
  #-}
