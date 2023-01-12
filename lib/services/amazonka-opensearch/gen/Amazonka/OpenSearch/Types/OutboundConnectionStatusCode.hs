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
-- Module      : Amazonka.OpenSearch.Types.OutboundConnectionStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OutboundConnectionStatusCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OutboundConnectionStatusCode = OutboundConnectionStatusCode'
  { fromOutboundConnectionStatusCode ::
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
