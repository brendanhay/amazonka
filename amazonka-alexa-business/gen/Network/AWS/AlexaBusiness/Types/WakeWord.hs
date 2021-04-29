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
-- Module      : Network.AWS.AlexaBusiness.Types.WakeWord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.WakeWord
  ( WakeWord
      ( ..,
        WakeWord_ALEXA,
        WakeWord_AMAZON,
        WakeWord_COMPUTER,
        WakeWord_ECHO
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype WakeWord = WakeWord'
  { fromWakeWord ::
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

pattern WakeWord_ALEXA :: WakeWord
pattern WakeWord_ALEXA = WakeWord' "ALEXA"

pattern WakeWord_AMAZON :: WakeWord
pattern WakeWord_AMAZON = WakeWord' "AMAZON"

pattern WakeWord_COMPUTER :: WakeWord
pattern WakeWord_COMPUTER = WakeWord' "COMPUTER"

pattern WakeWord_ECHO :: WakeWord
pattern WakeWord_ECHO = WakeWord' "ECHO"

{-# COMPLETE
  WakeWord_ALEXA,
  WakeWord_AMAZON,
  WakeWord_COMPUTER,
  WakeWord_ECHO,
  WakeWord'
  #-}
