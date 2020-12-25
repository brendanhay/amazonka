{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionFilterKey
  ( SessionFilterKey
      ( SessionFilterKey',
        SessionFilterKeyInvokedAfter,
        SessionFilterKeyInvokedBefore,
        SessionFilterKeyTarget,
        SessionFilterKeyOwner,
        SessionFilterKeyStatus,
        SessionFilterKeySessionId,
        fromSessionFilterKey
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SessionFilterKey = SessionFilterKey'
  { fromSessionFilterKey ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern SessionFilterKeyInvokedAfter :: SessionFilterKey
pattern SessionFilterKeyInvokedAfter = SessionFilterKey' "InvokedAfter"

pattern SessionFilterKeyInvokedBefore :: SessionFilterKey
pattern SessionFilterKeyInvokedBefore = SessionFilterKey' "InvokedBefore"

pattern SessionFilterKeyTarget :: SessionFilterKey
pattern SessionFilterKeyTarget = SessionFilterKey' "Target"

pattern SessionFilterKeyOwner :: SessionFilterKey
pattern SessionFilterKeyOwner = SessionFilterKey' "Owner"

pattern SessionFilterKeyStatus :: SessionFilterKey
pattern SessionFilterKeyStatus = SessionFilterKey' "Status"

pattern SessionFilterKeySessionId :: SessionFilterKey
pattern SessionFilterKeySessionId = SessionFilterKey' "SessionId"

{-# COMPLETE
  SessionFilterKeyInvokedAfter,
  SessionFilterKeyInvokedBefore,
  SessionFilterKeyTarget,
  SessionFilterKeyOwner,
  SessionFilterKeyStatus,
  SessionFilterKeySessionId,
  SessionFilterKey'
  #-}
