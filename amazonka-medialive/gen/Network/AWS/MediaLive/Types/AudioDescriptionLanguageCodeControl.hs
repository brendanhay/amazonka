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

import qualified Network.AWS.Prelude as Prelude

-- | Audio Description Language Code Control
newtype AudioDescriptionLanguageCodeControl = AudioDescriptionLanguageCodeControl'
  { fromAudioDescriptionLanguageCodeControl ::
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

pattern AudioDescriptionLanguageCodeControl_FOLLOW_INPUT :: AudioDescriptionLanguageCodeControl
pattern AudioDescriptionLanguageCodeControl_FOLLOW_INPUT = AudioDescriptionLanguageCodeControl' "FOLLOW_INPUT"

pattern AudioDescriptionLanguageCodeControl_USE_CONFIGURED :: AudioDescriptionLanguageCodeControl
pattern AudioDescriptionLanguageCodeControl_USE_CONFIGURED = AudioDescriptionLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE
  AudioDescriptionLanguageCodeControl_FOLLOW_INPUT,
  AudioDescriptionLanguageCodeControl_USE_CONFIGURED,
  AudioDescriptionLanguageCodeControl'
  #-}
