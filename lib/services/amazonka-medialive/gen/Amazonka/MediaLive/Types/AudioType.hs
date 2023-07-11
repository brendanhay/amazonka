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
-- Module      : Amazonka.MediaLive.Types.AudioType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.AudioType
  ( AudioType
      ( ..,
        AudioType_CLEAN_EFFECTS,
        AudioType_HEARING_IMPAIRED,
        AudioType_UNDEFINED,
        AudioType_VISUAL_IMPAIRED_COMMENTARY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Audio Type
newtype AudioType = AudioType'
  { fromAudioType ::
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
