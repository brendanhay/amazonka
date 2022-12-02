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
-- Module      : Amazonka.MediaLive.Types.AudioLanguageSelectionPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioLanguageSelectionPolicy
  ( AudioLanguageSelectionPolicy
      ( ..,
        AudioLanguageSelectionPolicy_LOOSE,
        AudioLanguageSelectionPolicy_STRICT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Audio Language Selection Policy
newtype AudioLanguageSelectionPolicy = AudioLanguageSelectionPolicy'
  { fromAudioLanguageSelectionPolicy ::
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

pattern AudioLanguageSelectionPolicy_LOOSE :: AudioLanguageSelectionPolicy
pattern AudioLanguageSelectionPolicy_LOOSE = AudioLanguageSelectionPolicy' "LOOSE"

pattern AudioLanguageSelectionPolicy_STRICT :: AudioLanguageSelectionPolicy
pattern AudioLanguageSelectionPolicy_STRICT = AudioLanguageSelectionPolicy' "STRICT"

{-# COMPLETE
  AudioLanguageSelectionPolicy_LOOSE,
  AudioLanguageSelectionPolicy_STRICT,
  AudioLanguageSelectionPolicy'
  #-}
