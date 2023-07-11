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
-- Module      : Amazonka.MediaConvert.Types.AudioLanguageCodeControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioLanguageCodeControl
  ( AudioLanguageCodeControl
      ( ..,
        AudioLanguageCodeControl_FOLLOW_INPUT,
        AudioLanguageCodeControl_USE_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify which source for language code takes precedence for this audio
-- track. When you choose Follow input (FOLLOW_INPUT), the service uses the
-- language code from the input track if it\'s present. If there\'s no
-- languge code on the input track, the service uses the code that you
-- specify in the setting Language code (languageCode or
-- customLanguageCode). When you choose Use configured (USE_CONFIGURED),
-- the service uses the language code that you specify.
newtype AudioLanguageCodeControl = AudioLanguageCodeControl'
  { fromAudioLanguageCodeControl ::
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

pattern AudioLanguageCodeControl_FOLLOW_INPUT :: AudioLanguageCodeControl
pattern AudioLanguageCodeControl_FOLLOW_INPUT = AudioLanguageCodeControl' "FOLLOW_INPUT"

pattern AudioLanguageCodeControl_USE_CONFIGURED :: AudioLanguageCodeControl
pattern AudioLanguageCodeControl_USE_CONFIGURED = AudioLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE
  AudioLanguageCodeControl_FOLLOW_INPUT,
  AudioLanguageCodeControl_USE_CONFIGURED,
  AudioLanguageCodeControl'
  #-}
