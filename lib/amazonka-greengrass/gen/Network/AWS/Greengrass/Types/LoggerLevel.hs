{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.LoggerLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.LoggerLevel
  ( LoggerLevel
      ( LoggerLevel',
        LLDebug,
        LLInfo,
        LLWarn,
        LLError,
        LLFatal
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LoggerLevel = LoggerLevel' Lude.Text
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

pattern LLDebug :: LoggerLevel
pattern LLDebug = LoggerLevel' "DEBUG"

pattern LLInfo :: LoggerLevel
pattern LLInfo = LoggerLevel' "INFO"

pattern LLWarn :: LoggerLevel
pattern LLWarn = LoggerLevel' "WARN"

pattern LLError :: LoggerLevel
pattern LLError = LoggerLevel' "ERROR"

pattern LLFatal :: LoggerLevel
pattern LLFatal = LoggerLevel' "FATAL"

{-# COMPLETE
  LLDebug,
  LLInfo,
  LLWarn,
  LLError,
  LLFatal,
  LoggerLevel'
  #-}
