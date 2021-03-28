{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PresetListBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.PresetListBy
  ( PresetListBy
    ( PresetListBy'
    , PresetListByName
    , PresetListByCreationDate
    , PresetListBySystem
    , fromPresetListBy
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Optional. When you request a list of presets, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
newtype PresetListBy = PresetListBy'{fromPresetListBy :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern PresetListByName :: PresetListBy
pattern PresetListByName = PresetListBy' "NAME"

pattern PresetListByCreationDate :: PresetListBy
pattern PresetListByCreationDate = PresetListBy' "CREATION_DATE"

pattern PresetListBySystem :: PresetListBy
pattern PresetListBySystem = PresetListBy' "SYSTEM"

{-# COMPLETE 
  PresetListByName,

  PresetListByCreationDate,

  PresetListBySystem,
  PresetListBy'
  #-}
