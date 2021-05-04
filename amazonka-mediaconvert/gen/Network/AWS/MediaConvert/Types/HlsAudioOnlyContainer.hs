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

import qualified Network.AWS.Prelude as Prelude

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport
-- Stream (M2TS) to create a file in an MPEG2-TS container. Keep the
-- default value Automatic (AUTOMATIC) to create a raw audio-only file with
-- no container. Regardless of the value that you specify here, if this
-- output has video, the service will place outputs into an MPEG2-TS
-- container.
newtype HlsAudioOnlyContainer = HlsAudioOnlyContainer'
  { fromHlsAudioOnlyContainer ::
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

pattern HlsAudioOnlyContainer_AUTOMATIC :: HlsAudioOnlyContainer
pattern HlsAudioOnlyContainer_AUTOMATIC = HlsAudioOnlyContainer' "AUTOMATIC"

pattern HlsAudioOnlyContainer_M2TS :: HlsAudioOnlyContainer
pattern HlsAudioOnlyContainer_M2TS = HlsAudioOnlyContainer' "M2TS"

{-# COMPLETE
  HlsAudioOnlyContainer_AUTOMATIC,
  HlsAudioOnlyContainer_M2TS,
  HlsAudioOnlyContainer'
  #-}
