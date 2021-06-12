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
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
  ( HlsAudioOnlyContainer
      ( ..,
        HlsAudioOnlyContainer_AUTOMATIC,
        HlsAudioOnlyContainer_M2TS
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport
-- Stream (M2TS) to create a file in an MPEG2-TS container. Keep the
-- default value Automatic (AUTOMATIC) to create a raw audio-only file with
-- no container. Regardless of the value that you specify here, if this
-- output has video, the service will place outputs into an MPEG2-TS
-- container.
newtype HlsAudioOnlyContainer = HlsAudioOnlyContainer'
  { fromHlsAudioOnlyContainer ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern HlsAudioOnlyContainer_AUTOMATIC :: HlsAudioOnlyContainer
pattern HlsAudioOnlyContainer_AUTOMATIC = HlsAudioOnlyContainer' "AUTOMATIC"

pattern HlsAudioOnlyContainer_M2TS :: HlsAudioOnlyContainer
pattern HlsAudioOnlyContainer_M2TS = HlsAudioOnlyContainer' "M2TS"

{-# COMPLETE
  HlsAudioOnlyContainer_AUTOMATIC,
  HlsAudioOnlyContainer_M2TS,
  HlsAudioOnlyContainer'
  #-}
