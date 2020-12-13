{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Info,
        Error,
        Warn,
        Disabled
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

pattern Info :: LogLevel
pattern Info = LogLevel' "INFO"

pattern Error :: LogLevel
pattern Error = LogLevel' "ERROR"

pattern Warn :: LogLevel
pattern Warn = LogLevel' "WARN"

pattern Disabled :: LogLevel
pattern Disabled = LogLevel' "DISABLED"

{-# COMPLETE
  Debug,
  Info,
  Error,
  Warn,
  Disabled,
  LogLevel'
  #-}
