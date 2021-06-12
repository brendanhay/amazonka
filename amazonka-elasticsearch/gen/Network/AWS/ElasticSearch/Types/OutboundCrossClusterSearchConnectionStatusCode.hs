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
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
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

import qualified Network.AWS.Core as Core

newtype OutboundCrossClusterSearchConnectionStatusCode = OutboundCrossClusterSearchConnectionStatusCode'
  { fromOutboundCrossClusterSearchConnectionStatusCode ::
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
