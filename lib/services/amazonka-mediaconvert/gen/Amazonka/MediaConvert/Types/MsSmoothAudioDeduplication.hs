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
-- Module      : Amazonka.MediaConvert.Types.MsSmoothAudioDeduplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MsSmoothAudioDeduplication
  ( MsSmoothAudioDeduplication
      ( ..,
        MsSmoothAudioDeduplication_COMBINE_DUPLICATE_STREAMS,
        MsSmoothAudioDeduplication_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings
-- across a Microsoft Smooth output group into a single audio stream.
newtype MsSmoothAudioDeduplication = MsSmoothAudioDeduplication'
  { fromMsSmoothAudioDeduplication ::
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

pattern MsSmoothAudioDeduplication_COMBINE_DUPLICATE_STREAMS :: MsSmoothAudioDeduplication
pattern MsSmoothAudioDeduplication_COMBINE_DUPLICATE_STREAMS = MsSmoothAudioDeduplication' "COMBINE_DUPLICATE_STREAMS"

pattern MsSmoothAudioDeduplication_NONE :: MsSmoothAudioDeduplication
pattern MsSmoothAudioDeduplication_NONE = MsSmoothAudioDeduplication' "NONE"

{-# COMPLETE
  MsSmoothAudioDeduplication_COMBINE_DUPLICATE_STREAMS,
  MsSmoothAudioDeduplication_NONE,
  MsSmoothAudioDeduplication'
  #-}
