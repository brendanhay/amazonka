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
-- Module      : Amazonka.MediaConvert.Types.HlsAudioOnlyContainer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.HlsAudioOnlyContainer
  ( HlsAudioOnlyContainer
      ( ..,
        HlsAudioOnlyContainer_AUTOMATIC,
        HlsAudioOnlyContainer_M2TS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport
-- Stream (M2TS) to create a file in an MPEG2-TS container. Keep the
-- default value Automatic (AUTOMATIC) to create a raw audio-only file with
-- no container. Regardless of the value that you specify here, if this
-- output has video, the service will place outputs into an MPEG2-TS
-- container.
newtype HlsAudioOnlyContainer = HlsAudioOnlyContainer'
  { fromHlsAudioOnlyContainer ::
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

pattern HlsAudioOnlyContainer_AUTOMATIC :: HlsAudioOnlyContainer
pattern HlsAudioOnlyContainer_AUTOMATIC = HlsAudioOnlyContainer' "AUTOMATIC"

pattern HlsAudioOnlyContainer_M2TS :: HlsAudioOnlyContainer
pattern HlsAudioOnlyContainer_M2TS = HlsAudioOnlyContainer' "M2TS"

{-# COMPLETE
  HlsAudioOnlyContainer_AUTOMATIC,
  HlsAudioOnlyContainer_M2TS,
  HlsAudioOnlyContainer'
  #-}
