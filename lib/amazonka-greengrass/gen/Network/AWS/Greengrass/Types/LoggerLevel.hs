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
        Debug,
        Error,
        Fatal,
        Info,
        Warn
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

pattern Debug :: LoggerLevel
pattern Debug = LoggerLevel' "DEBUG"

pattern Error :: LoggerLevel
pattern Error = LoggerLevel' "ERROR"

pattern Fatal :: LoggerLevel
pattern Fatal = LoggerLevel' "FATAL"

pattern Info :: LoggerLevel
pattern Info = LoggerLevel' "INFO"

pattern Warn :: LoggerLevel
pattern Warn = LoggerLevel' "WARN"

{-# COMPLETE
  Debug,
  Error,
  Fatal,
  Info,
  Warn,
  LoggerLevel'
  #-}
