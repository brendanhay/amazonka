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
-- Module      : Network.AWS.OpenSearch.Types.OutboundConnectionStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.OutboundConnectionStatusCode
  ( OutboundConnectionStatusCode
      ( ..,
        OutboundConnectionStatusCode_ACTIVE,
        OutboundConnectionStatusCode_APPROVED,
        OutboundConnectionStatusCode_DELETED,
        OutboundConnectionStatusCode_DELETING,
        OutboundConnectionStatusCode_PENDING_ACCEPTANCE,
        OutboundConnectionStatusCode_PROVISIONING,
        OutboundConnectionStatusCode_REJECTED,
        OutboundConnectionStatusCode_REJECTING,
        OutboundConnectionStatusCode_VALIDATING,
        OutboundConnectionStatusCode_VALIDATION_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype OutboundConnectionStatusCode = OutboundConnectionStatusCode'
  { fromOutboundConnectionStatusCode ::
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

pattern OutboundConnectionStatusCode_ACTIVE :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_ACTIVE = OutboundConnectionStatusCode' "ACTIVE"

pattern OutboundConnectionStatusCode_APPROVED :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_APPROVED = OutboundConnectionStatusCode' "APPROVED"

pattern OutboundConnectionStatusCode_DELETED :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_DELETED = OutboundConnectionStatusCode' "DELETED"

pattern OutboundConnectionStatusCode_DELETING :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_DELETING = OutboundConnectionStatusCode' "DELETING"

pattern OutboundConnectionStatusCode_PENDING_ACCEPTANCE :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_PENDING_ACCEPTANCE = OutboundConnectionStatusCode' "PENDING_ACCEPTANCE"

pattern OutboundConnectionStatusCode_PROVISIONING :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_PROVISIONING = OutboundConnectionStatusCode' "PROVISIONING"

pattern OutboundConnectionStatusCode_REJECTED :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_REJECTED = OutboundConnectionStatusCode' "REJECTED"

pattern OutboundConnectionStatusCode_REJECTING :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_REJECTING = OutboundConnectionStatusCode' "REJECTING"

pattern OutboundConnectionStatusCode_VALIDATING :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_VALIDATING = OutboundConnectionStatusCode' "VALIDATING"

pattern OutboundConnectionStatusCode_VALIDATION_FAILED :: OutboundConnectionStatusCode
pattern OutboundConnectionStatusCode_VALIDATION_FAILED = OutboundConnectionStatusCode' "VALIDATION_FAILED"

{-# COMPLETE
  OutboundConnectionStatusCode_ACTIVE,
  OutboundConnectionStatusCode_APPROVED,
  OutboundConnectionStatusCode_DELETED,
  OutboundConnectionStatusCode_DELETING,
  OutboundConnectionStatusCode_PENDING_ACCEPTANCE,
  OutboundConnectionStatusCode_PROVISIONING,
  OutboundConnectionStatusCode_REJECTED,
  OutboundConnectionStatusCode_REJECTING,
  OutboundConnectionStatusCode_VALIDATING,
  OutboundConnectionStatusCode_VALIDATION_FAILED,
  OutboundConnectionStatusCode'
  #-}
