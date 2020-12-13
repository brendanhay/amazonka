{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.UpdateAgentLogLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.UpdateAgentLogLevel
  ( UpdateAgentLogLevel
      ( UpdateAgentLogLevel',
        None,
        Trace,
        Debug,
        Verbose,
        Info,
        Warn,
        Error,
        Fatal
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The minimum level of log statements that should be logged by the OTA Agent during an update.
newtype UpdateAgentLogLevel = UpdateAgentLogLevel' Lude.Text
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

pattern None :: UpdateAgentLogLevel
pattern None = UpdateAgentLogLevel' "NONE"

pattern Trace :: UpdateAgentLogLevel
pattern Trace = UpdateAgentLogLevel' "TRACE"

pattern Debug :: UpdateAgentLogLevel
pattern Debug = UpdateAgentLogLevel' "DEBUG"

pattern Verbose :: UpdateAgentLogLevel
pattern Verbose = UpdateAgentLogLevel' "VERBOSE"

pattern Info :: UpdateAgentLogLevel
pattern Info = UpdateAgentLogLevel' "INFO"

pattern Warn :: UpdateAgentLogLevel
pattern Warn = UpdateAgentLogLevel' "WARN"

pattern Error :: UpdateAgentLogLevel
pattern Error = UpdateAgentLogLevel' "ERROR"

pattern Fatal :: UpdateAgentLogLevel
pattern Fatal = UpdateAgentLogLevel' "FATAL"

{-# COMPLETE
  None,
  Trace,
  Debug,
  Verbose,
  Info,
  Warn,
  Error,
  Fatal,
  UpdateAgentLogLevel'
  #-}
