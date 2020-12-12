{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroupType
  ( OutputGroupType
      ( OutputGroupType',
        CmafGroupSettings,
        DashIsoGroupSettings,
        FileGroupSettings,
        HlsGroupSettings,
        MsSmoothGroupSettings
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth Streaming, CMAF)
newtype OutputGroupType = OutputGroupType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CmafGroupSettings :: OutputGroupType
pattern CmafGroupSettings = OutputGroupType' "CMAF_GROUP_SETTINGS"

pattern DashIsoGroupSettings :: OutputGroupType
pattern DashIsoGroupSettings = OutputGroupType' "DASH_ISO_GROUP_SETTINGS"

pattern FileGroupSettings :: OutputGroupType
pattern FileGroupSettings = OutputGroupType' "FILE_GROUP_SETTINGS"

pattern HlsGroupSettings :: OutputGroupType
pattern HlsGroupSettings = OutputGroupType' "HLS_GROUP_SETTINGS"

pattern MsSmoothGroupSettings :: OutputGroupType
pattern MsSmoothGroupSettings = OutputGroupType' "MS_SMOOTH_GROUP_SETTINGS"

{-# COMPLETE
  CmafGroupSettings,
  DashIsoGroupSettings,
  FileGroupSettings,
  HlsGroupSettings,
  MsSmoothGroupSettings,
  OutputGroupType'
  #-}
