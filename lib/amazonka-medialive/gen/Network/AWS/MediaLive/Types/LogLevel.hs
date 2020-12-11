-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.LogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.LogLevel
  ( LogLevel
      ( LogLevel',
        LLDebug,
        LLDisabled,
        LLError,
        LLInfo,
        LLWarning
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The log level the user wants for their channel.
newtype LogLevel = LogLevel' Lude.Text
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

pattern LLDebug :: LogLevel
pattern LLDebug = LogLevel' "DEBUG"

pattern LLDisabled :: LogLevel
pattern LLDisabled = LogLevel' "DISABLED"

pattern LLError :: LogLevel
pattern LLError = LogLevel' "ERROR"

pattern LLInfo :: LogLevel
pattern LLInfo = LogLevel' "INFO"

pattern LLWarning :: LogLevel
pattern LLWarning = LogLevel' "WARNING"

{-# COMPLETE
  LLDebug,
  LLDisabled,
  LLError,
  LLInfo,
  LLWarning,
  LogLevel'
  #-}
