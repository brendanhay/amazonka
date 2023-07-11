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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Audio Description Language Code Control
newtype AudioDescriptionLanguageCodeControl = AudioDescriptionLanguageCodeControl'
  { fromAudioDescriptionLanguageCodeControl ::
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

pattern AudioDescriptionLanguageCodeControl_FOLLOW_INPUT :: AudioDescriptionLanguageCodeControl
pattern AudioDescriptionLanguageCodeControl_FOLLOW_INPUT = AudioDescriptionLanguageCodeControl' "FOLLOW_INPUT"

pattern AudioDescriptionLanguageCodeControl_USE_CONFIGURED :: AudioDescriptionLanguageCodeControl
pattern AudioDescriptionLanguageCodeControl_USE_CONFIGURED = AudioDescriptionLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE
  AudioDescriptionLanguageCodeControl_FOLLOW_INPUT,
  AudioDescriptionLanguageCodeControl_USE_CONFIGURED,
  AudioDescriptionLanguageCodeControl'
  #-}
