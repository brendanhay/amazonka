{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentType
  ( DocumentType
      ( DocumentType',
        DocumentTypeCommand,
        DocumentTypePolicy,
        DocumentTypeAutomation,
        DocumentTypeSession,
        DocumentTypePackage,
        DocumentTypeApplicationConfiguration,
        DocumentTypeApplicationConfigurationSchema,
        DocumentTypeDeploymentStrategy,
        DocumentTypeChangeCalendar,
        fromDocumentType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DocumentType = DocumentType' {fromDocumentType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DocumentTypeCommand :: DocumentType
pattern DocumentTypeCommand = DocumentType' "Command"

pattern DocumentTypePolicy :: DocumentType
pattern DocumentTypePolicy = DocumentType' "Policy"

pattern DocumentTypeAutomation :: DocumentType
pattern DocumentTypeAutomation = DocumentType' "Automation"

pattern DocumentTypeSession :: DocumentType
pattern DocumentTypeSession = DocumentType' "Session"

pattern DocumentTypePackage :: DocumentType
pattern DocumentTypePackage = DocumentType' "Package"

pattern DocumentTypeApplicationConfiguration :: DocumentType
pattern DocumentTypeApplicationConfiguration = DocumentType' "ApplicationConfiguration"

pattern DocumentTypeApplicationConfigurationSchema :: DocumentType
pattern DocumentTypeApplicationConfigurationSchema = DocumentType' "ApplicationConfigurationSchema"

pattern DocumentTypeDeploymentStrategy :: DocumentType
pattern DocumentTypeDeploymentStrategy = DocumentType' "DeploymentStrategy"

pattern DocumentTypeChangeCalendar :: DocumentType
pattern DocumentTypeChangeCalendar = DocumentType' "ChangeCalendar"

{-# COMPLETE
  DocumentTypeCommand,
  DocumentTypePolicy,
  DocumentTypeAutomation,
  DocumentTypeSession,
  DocumentTypePackage,
  DocumentTypeApplicationConfiguration,
  DocumentTypeApplicationConfigurationSchema,
  DocumentTypeDeploymentStrategy,
  DocumentTypeChangeCalendar,
  DocumentType'
  #-}
