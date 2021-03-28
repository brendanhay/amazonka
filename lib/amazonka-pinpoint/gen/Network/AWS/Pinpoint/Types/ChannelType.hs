{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.ChannelType
  ( ChannelType
    ( ChannelType'
    , ChannelTypePush
    , ChannelTypeGcm
    , ChannelTypeApns
    , ChannelTypeApnsSandbox
    , ChannelTypeApnsVoip
    , ChannelTypeApnsVoipSandbox
    , ChannelTypeAdm
    , ChannelTypeSms
    , ChannelTypeVoice
    , ChannelTypeEmail
    , ChannelTypeBaidu
    , ChannelTypeCustom
    , fromChannelType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ChannelType = ChannelType'{fromChannelType :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern ChannelTypePush :: ChannelType
pattern ChannelTypePush = ChannelType' "PUSH"

pattern ChannelTypeGcm :: ChannelType
pattern ChannelTypeGcm = ChannelType' "GCM"

pattern ChannelTypeApns :: ChannelType
pattern ChannelTypeApns = ChannelType' "APNS"

pattern ChannelTypeApnsSandbox :: ChannelType
pattern ChannelTypeApnsSandbox = ChannelType' "APNS_SANDBOX"

pattern ChannelTypeApnsVoip :: ChannelType
pattern ChannelTypeApnsVoip = ChannelType' "APNS_VOIP"

pattern ChannelTypeApnsVoipSandbox :: ChannelType
pattern ChannelTypeApnsVoipSandbox = ChannelType' "APNS_VOIP_SANDBOX"

pattern ChannelTypeAdm :: ChannelType
pattern ChannelTypeAdm = ChannelType' "ADM"

pattern ChannelTypeSms :: ChannelType
pattern ChannelTypeSms = ChannelType' "SMS"

pattern ChannelTypeVoice :: ChannelType
pattern ChannelTypeVoice = ChannelType' "VOICE"

pattern ChannelTypeEmail :: ChannelType
pattern ChannelTypeEmail = ChannelType' "EMAIL"

pattern ChannelTypeBaidu :: ChannelType
pattern ChannelTypeBaidu = ChannelType' "BAIDU"

pattern ChannelTypeCustom :: ChannelType
pattern ChannelTypeCustom = ChannelType' "CUSTOM"

{-# COMPLETE 
  ChannelTypePush,

  ChannelTypeGcm,

  ChannelTypeApns,

  ChannelTypeApnsSandbox,

  ChannelTypeApnsVoip,

  ChannelTypeApnsVoipSandbox,

  ChannelTypeAdm,

  ChannelTypeSms,

  ChannelTypeVoice,

  ChannelTypeEmail,

  ChannelTypeBaidu,

  ChannelTypeCustom,
  ChannelType'
  #-}
