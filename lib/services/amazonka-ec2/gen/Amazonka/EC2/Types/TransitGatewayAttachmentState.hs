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
-- Module      : Amazonka.EC2.Types.TransitGatewayAttachmentState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayAttachmentState
  ( TransitGatewayAttachmentState
      ( ..,
        TransitGatewayAttachmentState_Available,
        TransitGatewayAttachmentState_Deleted,
        TransitGatewayAttachmentState_Deleting,
        TransitGatewayAttachmentState_Failed,
        TransitGatewayAttachmentState_Failing,
        TransitGatewayAttachmentState_Initiating,
        TransitGatewayAttachmentState_InitiatingRequest,
        TransitGatewayAttachmentState_Modifying,
        TransitGatewayAttachmentState_Pending,
        TransitGatewayAttachmentState_PendingAcceptance,
        TransitGatewayAttachmentState_Rejected,
        TransitGatewayAttachmentState_Rejecting,
        TransitGatewayAttachmentState_RollingBack
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype TransitGatewayAttachmentState = TransitGatewayAttachmentState'
  { fromTransitGatewayAttachmentState ::
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

pattern TransitGatewayAttachmentState_Available :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Available = TransitGatewayAttachmentState' "available"

pattern TransitGatewayAttachmentState_Deleted :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Deleted = TransitGatewayAttachmentState' "deleted"

pattern TransitGatewayAttachmentState_Deleting :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Deleting = TransitGatewayAttachmentState' "deleting"

pattern TransitGatewayAttachmentState_Failed :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Failed = TransitGatewayAttachmentState' "failed"

pattern TransitGatewayAttachmentState_Failing :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Failing = TransitGatewayAttachmentState' "failing"

pattern TransitGatewayAttachmentState_Initiating :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Initiating = TransitGatewayAttachmentState' "initiating"

pattern TransitGatewayAttachmentState_InitiatingRequest :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_InitiatingRequest = TransitGatewayAttachmentState' "initiatingRequest"

pattern TransitGatewayAttachmentState_Modifying :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Modifying = TransitGatewayAttachmentState' "modifying"

pattern TransitGatewayAttachmentState_Pending :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Pending = TransitGatewayAttachmentState' "pending"

pattern TransitGatewayAttachmentState_PendingAcceptance :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_PendingAcceptance = TransitGatewayAttachmentState' "pendingAcceptance"

pattern TransitGatewayAttachmentState_Rejected :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Rejected = TransitGatewayAttachmentState' "rejected"

pattern TransitGatewayAttachmentState_Rejecting :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_Rejecting = TransitGatewayAttachmentState' "rejecting"

pattern TransitGatewayAttachmentState_RollingBack :: TransitGatewayAttachmentState
pattern TransitGatewayAttachmentState_RollingBack = TransitGatewayAttachmentState' "rollingBack"

{-# COMPLETE
  TransitGatewayAttachmentState_Available,
  TransitGatewayAttachmentState_Deleted,
  TransitGatewayAttachmentState_Deleting,
  TransitGatewayAttachmentState_Failed,
  TransitGatewayAttachmentState_Failing,
  TransitGatewayAttachmentState_Initiating,
  TransitGatewayAttachmentState_InitiatingRequest,
  TransitGatewayAttachmentState_Modifying,
  TransitGatewayAttachmentState_Pending,
  TransitGatewayAttachmentState_PendingAcceptance,
  TransitGatewayAttachmentState_Rejected,
  TransitGatewayAttachmentState_Rejecting,
  TransitGatewayAttachmentState_RollingBack,
  TransitGatewayAttachmentState'
  #-}
