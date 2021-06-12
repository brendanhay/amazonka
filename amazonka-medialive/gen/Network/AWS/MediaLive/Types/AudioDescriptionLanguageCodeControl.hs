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
-- Module      : Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioDescriptionLanguageCodeControl
  ( AudioDescriptionLanguageCodeControl
      ( ..,
        AudioDescriptionLanguageCodeControl_FOLLOW_INPUT,
        AudioDescriptionLanguageCodeControl_USE_CONFIGURED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Audio Description Language Code Control
newtype AudioDescriptionLanguageCodeControl = AudioDescriptionLanguageCodeControl'
  { fromAudioDescriptionLanguageCodeControl ::
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

pattern AudioDescriptionLanguageCodeControl_FOLLOW_INPUT :: AudioDescriptionLanguageCodeControl
pattern AudioDescriptionLanguageCodeControl_FOLLOW_INPUT = AudioDescriptionLanguageCodeControl' "FOLLOW_INPUT"

pattern AudioDescriptionLanguageCodeControl_USE_CONFIGURED :: AudioDescriptionLanguageCodeControl
pattern AudioDescriptionLanguageCodeControl_USE_CONFIGURED = AudioDescriptionLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE
  AudioDescriptionLanguageCodeControl_FOLLOW_INPUT,
  AudioDescriptionLanguageCodeControl_USE_CONFIGURED,
  AudioDescriptionLanguageCodeControl'
  #-}
