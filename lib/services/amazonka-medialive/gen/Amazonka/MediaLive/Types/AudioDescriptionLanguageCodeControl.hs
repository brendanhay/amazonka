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
-- Module      : Amazonka.MediaLive.Types.AudioDescriptionLanguageCodeControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioDescriptionLanguageCodeControl
  ( AudioDescriptionLanguageCodeControl
      ( ..,
        AudioDescriptionLanguageCodeControl_FOLLOW_INPUT,
        AudioDescriptionLanguageCodeControl_USE_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Audio Description Language Code Control
newtype AudioDescriptionLanguageCodeControl = AudioDescriptionLanguageCodeControl'
  { fromAudioDescriptionLanguageCodeControl ::
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

pattern AudioDescriptionLanguageCodeControl_FOLLOW_INPUT :: AudioDescriptionLanguageCodeControl
pattern AudioDescriptionLanguageCodeControl_FOLLOW_INPUT = AudioDescriptionLanguageCodeControl' "FOLLOW_INPUT"

pattern AudioDescriptionLanguageCodeControl_USE_CONFIGURED :: AudioDescriptionLanguageCodeControl
pattern AudioDescriptionLanguageCodeControl_USE_CONFIGURED = AudioDescriptionLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE
  AudioDescriptionLanguageCodeControl_FOLLOW_INPUT,
  AudioDescriptionLanguageCodeControl_USE_CONFIGURED,
  AudioDescriptionLanguageCodeControl'
  #-}
