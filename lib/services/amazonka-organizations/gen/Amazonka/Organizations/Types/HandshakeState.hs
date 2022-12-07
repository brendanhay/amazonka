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
-- Module      : Amazonka.Organizations.Types.HandshakeState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.HandshakeState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HandshakeState = HandshakeState'
  { fromHandshakeState ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
