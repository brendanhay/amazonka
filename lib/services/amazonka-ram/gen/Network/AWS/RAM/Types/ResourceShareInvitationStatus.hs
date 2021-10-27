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
-- Module      : Network.AWS.RAM.Types.ResourceShareInvitationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RAM.Types.ResourceShareInvitationStatus
  ( ResourceShareInvitationStatus
      ( ..,
        ResourceShareInvitationStatus_ACCEPTED,
        ResourceShareInvitationStatus_EXPIRED,
        ResourceShareInvitationStatus_PENDING,
        ResourceShareInvitationStatus_REJECTED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResourceShareInvitationStatus = ResourceShareInvitationStatus'
  { fromResourceShareInvitationStatus ::
      Core.Text
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
