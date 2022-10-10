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
-- Module      : Amazonka.MediaConvert.Types.AudioDefaultSelection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioDefaultSelection
  ( AudioDefaultSelection
      ( ..,
        AudioDefaultSelection_DEFAULT,
        AudioDefaultSelection_NOT_DEFAULT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Enable this setting on one audio selector to set it as the default for
-- the job. The service uses this default for outputs where it can\'t find
-- the specified input audio. If you don\'t set a default, those outputs
-- have no audio.
newtype AudioDefaultSelection = AudioDefaultSelection'
  { fromAudioDefaultSelection ::
      Core.Text
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

pattern AudioDefaultSelection_DEFAULT :: AudioDefaultSelection
pattern AudioDefaultSelection_DEFAULT = AudioDefaultSelection' "DEFAULT"

pattern AudioDefaultSelection_NOT_DEFAULT :: AudioDefaultSelection
pattern AudioDefaultSelection_NOT_DEFAULT = AudioDefaultSelection' "NOT_DEFAULT"

{-# COMPLETE
  AudioDefaultSelection_DEFAULT,
  AudioDefaultSelection_NOT_DEFAULT,
  AudioDefaultSelection'
  #-}
