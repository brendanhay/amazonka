{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Audio Type
newtype AudioType = AudioType'
  { fromAudioType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
