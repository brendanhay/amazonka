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
-- Module      : Network.AWS.SSM.Types.SessionFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionFilterKey
  ( SessionFilterKey
      ( ..,
        SessionFilterKey_InvokedAfter,
        SessionFilterKey_InvokedBefore,
        SessionFilterKey_Owner,
        SessionFilterKey_SessionId,
        SessionFilterKey_Status,
        SessionFilterKey_Target
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SessionFilterKey = SessionFilterKey'
  { fromSessionFilterKey ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern SessionFilterKey_InvokedAfter :: SessionFilterKey
pattern SessionFilterKey_InvokedAfter = SessionFilterKey' "InvokedAfter"

pattern SessionFilterKey_InvokedBefore :: SessionFilterKey
pattern SessionFilterKey_InvokedBefore = SessionFilterKey' "InvokedBefore"

pattern SessionFilterKey_Owner :: SessionFilterKey
pattern SessionFilterKey_Owner = SessionFilterKey' "Owner"

pattern SessionFilterKey_SessionId :: SessionFilterKey
pattern SessionFilterKey_SessionId = SessionFilterKey' "SessionId"

pattern SessionFilterKey_Status :: SessionFilterKey
pattern SessionFilterKey_Status = SessionFilterKey' "Status"

pattern SessionFilterKey_Target :: SessionFilterKey
pattern SessionFilterKey_Target = SessionFilterKey' "Target"

{-# COMPLETE
  SessionFilterKey_InvokedAfter,
  SessionFilterKey_InvokedBefore,
  SessionFilterKey_Owner,
  SessionFilterKey_SessionId,
  SessionFilterKey_Status,
  SessionFilterKey_Target,
  SessionFilterKey'
  #-}
