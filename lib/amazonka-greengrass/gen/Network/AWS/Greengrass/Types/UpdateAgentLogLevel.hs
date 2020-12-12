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
        UALLDebug,
        UALLError,
        UALLFatal,
        UALLInfo,
        UALLNone,
        UALLTrace,
        UALLVerbose,
        UALLWarn
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

pattern UALLDebug :: UpdateAgentLogLevel
pattern UALLDebug = UpdateAgentLogLevel' "DEBUG"

pattern UALLError :: UpdateAgentLogLevel
pattern UALLError = UpdateAgentLogLevel' "ERROR"

pattern UALLFatal :: UpdateAgentLogLevel
pattern UALLFatal = UpdateAgentLogLevel' "FATAL"

pattern UALLInfo :: UpdateAgentLogLevel
pattern UALLInfo = UpdateAgentLogLevel' "INFO"

pattern UALLNone :: UpdateAgentLogLevel
pattern UALLNone = UpdateAgentLogLevel' "NONE"

pattern UALLTrace :: UpdateAgentLogLevel
pattern UALLTrace = UpdateAgentLogLevel' "TRACE"

pattern UALLVerbose :: UpdateAgentLogLevel
pattern UALLVerbose = UpdateAgentLogLevel' "VERBOSE"

pattern UALLWarn :: UpdateAgentLogLevel
pattern UALLWarn = UpdateAgentLogLevel' "WARN"

{-# COMPLETE
  UALLDebug,
  UALLError,
  UALLFatal,
  UALLInfo,
  UALLNone,
  UALLTrace,
  UALLVerbose,
  UALLWarn,
  UpdateAgentLogLevel'
  #-}
