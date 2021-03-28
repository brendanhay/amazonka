{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ExportType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.ExportType
  ( ExportType
    ( ExportType'
    , ExportTypeAlexaSkillsKit
    , ExportTypeLex
    , fromExportType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ExportType = ExportType'{fromExportType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern ExportTypeAlexaSkillsKit :: ExportType
pattern ExportTypeAlexaSkillsKit = ExportType' "ALEXA_SKILLS_KIT"

pattern ExportTypeLex :: ExportType
pattern ExportTypeLex = ExportType' "LEX"

{-# COMPLETE 
  ExportTypeAlexaSkillsKit,

  ExportTypeLex,
  ExportType'
  #-}
