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
-- Module      : Network.AWS.MediaConvert.Types.AudioDefaultSelection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioDefaultSelection
  ( AudioDefaultSelection
      ( ..,
        AudioDefaultSelection_DEFAULT,
        AudioDefaultSelection_NOT_DEFAULT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Enable this setting on one audio selector to set it as the default for
-- the job. The service uses this default for outputs where it can\'t find
-- the specified input audio. If you don\'t set a default, those outputs
-- have no audio.
newtype AudioDefaultSelection = AudioDefaultSelection'
  { fromAudioDefaultSelection ::
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

pattern AudioDefaultSelection_DEFAULT :: AudioDefaultSelection
pattern AudioDefaultSelection_DEFAULT = AudioDefaultSelection' "DEFAULT"

pattern AudioDefaultSelection_NOT_DEFAULT :: AudioDefaultSelection
pattern AudioDefaultSelection_NOT_DEFAULT = AudioDefaultSelection' "NOT_DEFAULT"

{-# COMPLETE
  AudioDefaultSelection_DEFAULT,
  AudioDefaultSelection_NOT_DEFAULT,
  AudioDefaultSelection'
  #-}
