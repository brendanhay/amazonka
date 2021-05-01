{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype OutboundCrossClusterSearchConnectionStatusCode = OutboundCrossClusterSearchConnectionStatusCode'
  { fromOutboundCrossClusterSearchConnectionStatusCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
