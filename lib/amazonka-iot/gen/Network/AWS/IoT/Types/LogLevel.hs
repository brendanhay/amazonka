-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogLevel
  ( LogLevel
      ( LogLevel',
        Debug,
        Disabled,
        Error,
        Info,
        Warn
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

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

pattern Debug :: LogLevel
pattern Debug = LogLevel' "DEBUG"

pattern Disabled :: LogLevel
pattern Disabled = LogLevel' "DISABLED"

pattern Error :: LogLevel
pattern Error = LogLevel' "ERROR"

pattern Info :: LogLevel
pattern Info = LogLevel' "INFO"

pattern Warn :: LogLevel
pattern Warn = LogLevel' "WARN"

{-# COMPLETE
  Debug,
  Disabled,
  Error,
  Info,
  Warn,
  LogLevel'
  #-}
