{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioLanguageCodeControl
  ( AudioLanguageCodeControl
      ( AudioLanguageCodeControl',
        FollowInput,
        UseConfigured
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify which source for language code takes precedence for this audio track. When you choose Follow input (FOLLOW_INPUT), the service uses the language code from the input track if it's present. If there's no languge code on the input track, the service uses the code that you specify in the setting Language code (languageCode or customLanguageCode). When you choose Use configured (USE_CONFIGURED), the service uses the language code that you specify.
newtype AudioLanguageCodeControl = AudioLanguageCodeControl' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern FollowInput :: AudioLanguageCodeControl
pattern FollowInput = AudioLanguageCodeControl' "FOLLOW_INPUT"

pattern UseConfigured :: AudioLanguageCodeControl
pattern UseConfigured = AudioLanguageCodeControl' "USE_CONFIGURED"

{-# COMPLETE
  FollowInput,
  UseConfigured,
  AudioLanguageCodeControl'
  #-}
