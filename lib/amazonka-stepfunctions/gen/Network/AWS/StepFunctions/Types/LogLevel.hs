-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LogLevel
  ( LogLevel
      ( LogLevel',
        All,
        Error,
        Fatal,
        Off
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

pattern All :: LogLevel
pattern All = LogLevel' "ALL"

pattern Error :: LogLevel
pattern Error = LogLevel' "ERROR"

pattern Fatal :: LogLevel
pattern Fatal = LogLevel' "FATAL"

pattern Off :: LogLevel
pattern Off = LogLevel' "OFF"

{-# COMPLETE
  All,
  Error,
  Fatal,
  Off,
  LogLevel'
  #-}
