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
        Alexa,
        Amazon,
        Computer,
        Echo
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WakeWord = WakeWord' Lude.Text
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

pattern Alexa :: WakeWord
pattern Alexa = WakeWord' "ALEXA"

pattern Amazon :: WakeWord
pattern Amazon = WakeWord' "AMAZON"

pattern Computer :: WakeWord
pattern Computer = WakeWord' "COMPUTER"

pattern Echo :: WakeWord
pattern Echo = WakeWord' "ECHO"

{-# COMPLETE
  Alexa,
  Amazon,
  Computer,
  Echo,
  WakeWord'
  #-}
