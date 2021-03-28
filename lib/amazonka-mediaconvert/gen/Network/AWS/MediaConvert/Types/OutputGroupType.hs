{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.OutputGroupType
  ( OutputGroupType
    ( OutputGroupType'
    , OutputGroupTypeHlsGroupSettings
    , OutputGroupTypeDashIsoGroupSettings
    , OutputGroupTypeFileGroupSettings
    , OutputGroupTypeMsSmoothGroupSettings
    , OutputGroupTypeCmafGroupSettings
    , fromOutputGroupType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming, CMAF)
newtype OutputGroupType = OutputGroupType'{fromOutputGroupType ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern OutputGroupTypeHlsGroupSettings :: OutputGroupType
pattern OutputGroupTypeHlsGroupSettings = OutputGroupType' "HLS_GROUP_SETTINGS"

pattern OutputGroupTypeDashIsoGroupSettings :: OutputGroupType
pattern OutputGroupTypeDashIsoGroupSettings = OutputGroupType' "DASH_ISO_GROUP_SETTINGS"

pattern OutputGroupTypeFileGroupSettings :: OutputGroupType
pattern OutputGroupTypeFileGroupSettings = OutputGroupType' "FILE_GROUP_SETTINGS"

pattern OutputGroupTypeMsSmoothGroupSettings :: OutputGroupType
pattern OutputGroupTypeMsSmoothGroupSettings = OutputGroupType' "MS_SMOOTH_GROUP_SETTINGS"

pattern OutputGroupTypeCmafGroupSettings :: OutputGroupType
pattern OutputGroupTypeCmafGroupSettings = OutputGroupType' "CMAF_GROUP_SETTINGS"

{-# COMPLETE 
  OutputGroupTypeHlsGroupSettings,

  OutputGroupTypeDashIsoGroupSettings,

  OutputGroupTypeFileGroupSettings,

  OutputGroupTypeMsSmoothGroupSettings,

  OutputGroupTypeCmafGroupSettings,
  OutputGroupType'
  #-}
