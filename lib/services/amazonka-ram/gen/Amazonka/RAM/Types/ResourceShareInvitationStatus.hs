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
-- Module      : Amazonka.RAM.Types.ResourceShareInvitationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceShareInvitationStatus
  ( ResourceShareInvitationStatus
      ( ..,
        ResourceShareInvitationStatus_ACCEPTED,
        ResourceShareInvitationStatus_EXPIRED,
        ResourceShareInvitationStatus_PENDING,
        ResourceShareInvitationStatus_REJECTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceShareInvitationStatus = ResourceShareInvitationStatus'
  { fromResourceShareInvitationStatus ::
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

pattern ResourceShareInvitationStatus_ACCEPTED :: ResourceShareInvitationStatus
pattern ResourceShareInvitationStatus_ACCEPTED = ResourceShareInvitationStatus' "ACCEPTED"

pattern ResourceShareInvitationStatus_EXPIRED :: ResourceShareInvitationStatus
pattern ResourceShareInvitationStatus_EXPIRED = ResourceShareInvitationStatus' "EXPIRED"

pattern ResourceShareInvitationStatus_PENDING :: ResourceShareInvitationStatus
pattern ResourceShareInvitationStatus_PENDING = ResourceShareInvitationStatus' "PENDING"

pattern ResourceShareInvitationStatus_REJECTED :: ResourceShareInvitationStatus
pattern ResourceShareInvitationStatus_REJECTED = ResourceShareInvitationStatus' "REJECTED"

{-# COMPLETE
  ResourceShareInvitationStatus_ACCEPTED,
  ResourceShareInvitationStatus_EXPIRED,
  ResourceShareInvitationStatus_PENDING,
  ResourceShareInvitationStatus_REJECTED,
  ResourceShareInvitationStatus'
  #-}
