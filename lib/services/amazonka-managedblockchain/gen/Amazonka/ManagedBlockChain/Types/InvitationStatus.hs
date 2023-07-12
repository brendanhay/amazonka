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
-- Module      : Amazonka.ManagedBlockChain.Types.InvitationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.InvitationStatus
  ( InvitationStatus
      ( ..,
        InvitationStatus_ACCEPTED,
        InvitationStatus_ACCEPTING,
        InvitationStatus_EXPIRED,
        InvitationStatus_PENDING,
        InvitationStatus_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InvitationStatus = InvitationStatus'
  { fromInvitationStatus ::
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

pattern InvitationStatus_ACCEPTED :: InvitationStatus
pattern InvitationStatus_ACCEPTED = InvitationStatus' "ACCEPTED"

pattern InvitationStatus_ACCEPTING :: InvitationStatus
pattern InvitationStatus_ACCEPTING = InvitationStatus' "ACCEPTING"

pattern InvitationStatus_EXPIRED :: InvitationStatus
pattern InvitationStatus_EXPIRED = InvitationStatus' "EXPIRED"

pattern InvitationStatus_PENDING :: InvitationStatus
pattern InvitationStatus_PENDING = InvitationStatus' "PENDING"

pattern InvitationStatus_REJECTED :: InvitationStatus
pattern InvitationStatus_REJECTED = InvitationStatus' "REJECTED"

{-# COMPLETE
  InvitationStatus_ACCEPTED,
  InvitationStatus_ACCEPTING,
  InvitationStatus_EXPIRED,
  InvitationStatus_PENDING,
  InvitationStatus_REJECTED,
  InvitationStatus'
  #-}
