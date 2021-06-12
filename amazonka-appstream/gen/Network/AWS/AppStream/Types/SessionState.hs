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
-- Module      : Network.AWS.AppStream.Types.SessionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.SessionState
  ( SessionState
      ( ..,
        SessionState_ACTIVE,
        SessionState_EXPIRED,
        SessionState_PENDING
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Possible values for the state of a streaming session.
newtype SessionState = SessionState'
  { fromSessionState ::
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

pattern SessionState_ACTIVE :: SessionState
pattern SessionState_ACTIVE = SessionState' "ACTIVE"

pattern SessionState_EXPIRED :: SessionState
pattern SessionState_EXPIRED = SessionState' "EXPIRED"

pattern SessionState_PENDING :: SessionState
pattern SessionState_PENDING = SessionState' "PENDING"

{-# COMPLETE
  SessionState_ACTIVE,
  SessionState_EXPIRED,
  SessionState_PENDING,
  SessionState'
  #-}
