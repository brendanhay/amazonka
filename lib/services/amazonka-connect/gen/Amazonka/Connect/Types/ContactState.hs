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
-- Module      : Amazonka.Connect.Types.ContactState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ContactState
  ( ContactState
      ( ..,
        ContactState_CONNECTED,
        ContactState_CONNECTED_ONHOLD,
        ContactState_CONNECTING,
        ContactState_ENDED,
        ContactState_ERROR,
        ContactState_INCOMING,
        ContactState_MISSED,
        ContactState_PENDING,
        ContactState_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContactState = ContactState'
  { fromContactState ::
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

pattern ContactState_CONNECTED :: ContactState
pattern ContactState_CONNECTED = ContactState' "CONNECTED"

pattern ContactState_CONNECTED_ONHOLD :: ContactState
pattern ContactState_CONNECTED_ONHOLD = ContactState' "CONNECTED_ONHOLD"

pattern ContactState_CONNECTING :: ContactState
pattern ContactState_CONNECTING = ContactState' "CONNECTING"

pattern ContactState_ENDED :: ContactState
pattern ContactState_ENDED = ContactState' "ENDED"

pattern ContactState_ERROR :: ContactState
pattern ContactState_ERROR = ContactState' "ERROR"

pattern ContactState_INCOMING :: ContactState
pattern ContactState_INCOMING = ContactState' "INCOMING"

pattern ContactState_MISSED :: ContactState
pattern ContactState_MISSED = ContactState' "MISSED"

pattern ContactState_PENDING :: ContactState
pattern ContactState_PENDING = ContactState' "PENDING"

pattern ContactState_REJECTED :: ContactState
pattern ContactState_REJECTED = ContactState' "REJECTED"

{-# COMPLETE
  ContactState_CONNECTED,
  ContactState_CONNECTED_ONHOLD,
  ContactState_CONNECTING,
  ContactState_ENDED,
  ContactState_ERROR,
  ContactState_INCOMING,
  ContactState_MISSED,
  ContactState_PENDING,
  ContactState_REJECTED,
  ContactState'
  #-}
