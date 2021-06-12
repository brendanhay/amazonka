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
-- Module      : Network.AWS.Organizations.Types.HandshakeState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeState
  ( HandshakeState
      ( ..,
        HandshakeState_ACCEPTED,
        HandshakeState_CANCELED,
        HandshakeState_DECLINED,
        HandshakeState_EXPIRED,
        HandshakeState_OPEN,
        HandshakeState_REQUESTED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HandshakeState = HandshakeState'
  { fromHandshakeState ::
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

pattern HandshakeState_ACCEPTED :: HandshakeState
pattern HandshakeState_ACCEPTED = HandshakeState' "ACCEPTED"

pattern HandshakeState_CANCELED :: HandshakeState
pattern HandshakeState_CANCELED = HandshakeState' "CANCELED"

pattern HandshakeState_DECLINED :: HandshakeState
pattern HandshakeState_DECLINED = HandshakeState' "DECLINED"

pattern HandshakeState_EXPIRED :: HandshakeState
pattern HandshakeState_EXPIRED = HandshakeState' "EXPIRED"

pattern HandshakeState_OPEN :: HandshakeState
pattern HandshakeState_OPEN = HandshakeState' "OPEN"

pattern HandshakeState_REQUESTED :: HandshakeState
pattern HandshakeState_REQUESTED = HandshakeState' "REQUESTED"

{-# COMPLETE
  HandshakeState_ACCEPTED,
  HandshakeState_CANCELED,
  HandshakeState_DECLINED,
  HandshakeState_EXPIRED,
  HandshakeState_OPEN,
  HandshakeState_REQUESTED,
  HandshakeState'
  #-}
