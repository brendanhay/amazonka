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
-- Module      : Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
  ( OutboundCrossClusterSearchConnectionStatusCode
      ( ..,
        OutboundCrossClusterSearchConnectionStatusCode_ACTIVE,
        OutboundCrossClusterSearchConnectionStatusCode_DELETED,
        OutboundCrossClusterSearchConnectionStatusCode_DELETING,
        OutboundCrossClusterSearchConnectionStatusCode_PENDING_ACCEPTANCE,
        OutboundCrossClusterSearchConnectionStatusCode_PROVISIONING,
        OutboundCrossClusterSearchConnectionStatusCode_REJECTED,
        OutboundCrossClusterSearchConnectionStatusCode_VALIDATING,
        OutboundCrossClusterSearchConnectionStatusCode_VALIDATION_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OutboundCrossClusterSearchConnectionStatusCode = OutboundCrossClusterSearchConnectionStatusCode'
  { fromOutboundCrossClusterSearchConnectionStatusCode ::
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

pattern OutboundCrossClusterSearchConnectionStatusCode_ACTIVE :: OutboundCrossClusterSearchConnectionStatusCode
pattern OutboundCrossClusterSearchConnectionStatusCode_ACTIVE = OutboundCrossClusterSearchConnectionStatusCode' "ACTIVE"

pattern OutboundCrossClusterSearchConnectionStatusCode_DELETED :: OutboundCrossClusterSearchConnectionStatusCode
pattern OutboundCrossClusterSearchConnectionStatusCode_DELETED = OutboundCrossClusterSearchConnectionStatusCode' "DELETED"

pattern OutboundCrossClusterSearchConnectionStatusCode_DELETING :: OutboundCrossClusterSearchConnectionStatusCode
pattern OutboundCrossClusterSearchConnectionStatusCode_DELETING = OutboundCrossClusterSearchConnectionStatusCode' "DELETING"

pattern OutboundCrossClusterSearchConnectionStatusCode_PENDING_ACCEPTANCE :: OutboundCrossClusterSearchConnectionStatusCode
pattern OutboundCrossClusterSearchConnectionStatusCode_PENDING_ACCEPTANCE = OutboundCrossClusterSearchConnectionStatusCode' "PENDING_ACCEPTANCE"

pattern OutboundCrossClusterSearchConnectionStatusCode_PROVISIONING :: OutboundCrossClusterSearchConnectionStatusCode
pattern OutboundCrossClusterSearchConnectionStatusCode_PROVISIONING = OutboundCrossClusterSearchConnectionStatusCode' "PROVISIONING"

pattern OutboundCrossClusterSearchConnectionStatusCode_REJECTED :: OutboundCrossClusterSearchConnectionStatusCode
pattern OutboundCrossClusterSearchConnectionStatusCode_REJECTED = OutboundCrossClusterSearchConnectionStatusCode' "REJECTED"

pattern OutboundCrossClusterSearchConnectionStatusCode_VALIDATING :: OutboundCrossClusterSearchConnectionStatusCode
pattern OutboundCrossClusterSearchConnectionStatusCode_VALIDATING = OutboundCrossClusterSearchConnectionStatusCode' "VALIDATING"

pattern OutboundCrossClusterSearchConnectionStatusCode_VALIDATION_FAILED :: OutboundCrossClusterSearchConnectionStatusCode
pattern OutboundCrossClusterSearchConnectionStatusCode_VALIDATION_FAILED = OutboundCrossClusterSearchConnectionStatusCode' "VALIDATION_FAILED"

{-# COMPLETE
  OutboundCrossClusterSearchConnectionStatusCode_ACTIVE,
  OutboundCrossClusterSearchConnectionStatusCode_DELETED,
  OutboundCrossClusterSearchConnectionStatusCode_DELETING,
  OutboundCrossClusterSearchConnectionStatusCode_PENDING_ACCEPTANCE,
  OutboundCrossClusterSearchConnectionStatusCode_PROVISIONING,
  OutboundCrossClusterSearchConnectionStatusCode_REJECTED,
  OutboundCrossClusterSearchConnectionStatusCode_VALIDATING,
  OutboundCrossClusterSearchConnectionStatusCode_VALIDATION_FAILED,
  OutboundCrossClusterSearchConnectionStatusCode'
  #-}
