{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
  ( AudioLanguageCodeControl
    ( AudioLanguageCodeControl'
    , AudioLanguageCodeControlFollowInput
    , AudioLanguageCodeControlUseConfigured
    , fromAudioLanguageCodeControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
newtype AudioLanguageCodeControl = AudioLanguageCodeControl'{fromAudioLanguageCodeControl
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern AudioLanguageCodeControlFollowInput :: AudioLanguageCodeControl
pattern AudioLanguageCodeControlFollowInput = AudioLanguageCodeControl' "FOLLOW_INPUT"

pattern AudioLanguageCodeControlUseConfigured :: AudioLanguageCodeControl
pattern AudioLanguageCodeControlUseConfigured = AudioLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE 
  AudioLanguageCodeControlFollowInput,

  AudioLanguageCodeControlUseConfigured,
  AudioLanguageCodeControl'
  #-}
