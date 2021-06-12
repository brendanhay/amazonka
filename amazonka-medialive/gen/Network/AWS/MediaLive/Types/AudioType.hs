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
-- Module      : Network.AWS.MediaLive.Types.AudioType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioType
  ( AudioType
      ( ..,
        AudioType_CLEAN_EFFECTS,
        AudioType_HEARING_IMPAIRED,
        AudioType_UNDEFINED,
        AudioType_VISUAL_IMPAIRED_COMMENTARY
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Audio Type
newtype AudioType = AudioType'
  { fromAudioType ::
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

pattern AudioType_CLEAN_EFFECTS :: AudioType
pattern AudioType_CLEAN_EFFECTS = AudioType' "CLEAN_EFFECTS"

pattern AudioType_HEARING_IMPAIRED :: AudioType
pattern AudioType_HEARING_IMPAIRED = AudioType' "HEARING_IMPAIRED"

pattern AudioType_UNDEFINED :: AudioType
pattern AudioType_UNDEFINED = AudioType' "UNDEFINED"

pattern AudioType_VISUAL_IMPAIRED_COMMENTARY :: AudioType
pattern AudioType_VISUAL_IMPAIRED_COMMENTARY = AudioType' "VISUAL_IMPAIRED_COMMENTARY"

{-# COMPLETE
  AudioType_CLEAN_EFFECTS,
  AudioType_HEARING_IMPAIRED,
  AudioType_UNDEFINED,
  AudioType_VISUAL_IMPAIRED_COMMENTARY,
  AudioType'
  #-}
