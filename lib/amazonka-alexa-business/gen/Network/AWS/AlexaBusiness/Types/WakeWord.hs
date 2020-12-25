{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.WakeWord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.WakeWord
  ( WakeWord
      ( WakeWord',
        WakeWordAlexa,
        WakeWordAmazon,
        WakeWordEcho,
        WakeWordComputer,
        fromWakeWord
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype WakeWord = WakeWord' {fromWakeWord :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern WakeWordAlexa :: WakeWord
pattern WakeWordAlexa = WakeWord' "ALEXA"

pattern WakeWordAmazon :: WakeWord
pattern WakeWordAmazon = WakeWord' "AMAZON"

pattern WakeWordEcho :: WakeWord
pattern WakeWordEcho = WakeWord' "ECHO"

pattern WakeWordComputer :: WakeWord
pattern WakeWordComputer = WakeWord' "COMPUTER"

{-# COMPLETE
  WakeWordAlexa,
  WakeWordAmazon,
  WakeWordEcho,
  WakeWordComputer,
  WakeWord'
  #-}
