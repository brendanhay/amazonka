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
-- Module      : Amazonka.AlexaBusiness.Types.WakeWord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.WakeWord
  ( WakeWord
      ( ..,
        WakeWord_ALEXA,
        WakeWord_AMAZON,
        WakeWord_COMPUTER,
        WakeWord_ECHO
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype WakeWord = WakeWord'
  { fromWakeWord ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
