-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
  ( OutboundCrossClusterSearchConnectionStatusCode
      ( OutboundCrossClusterSearchConnectionStatusCode',
        OCCSCSCActive,
        OCCSCSCDeleted,
        OCCSCSCDeleting,
        OCCSCSCPendingAcceptance,
        OCCSCSCProvisioning,
        OCCSCSCRejected,
        OCCSCSCValidating,
        OCCSCSCValidationFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OutboundCrossClusterSearchConnectionStatusCode = OutboundCrossClusterSearchConnectionStatusCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OCCSCSCActive :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCActive = OutboundCrossClusterSearchConnectionStatusCode' "ACTIVE"

pattern OCCSCSCDeleted :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCDeleted = OutboundCrossClusterSearchConnectionStatusCode' "DELETED"

pattern OCCSCSCDeleting :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCDeleting = OutboundCrossClusterSearchConnectionStatusCode' "DELETING"

pattern OCCSCSCPendingAcceptance :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCPendingAcceptance = OutboundCrossClusterSearchConnectionStatusCode' "PENDING_ACCEPTANCE"

pattern OCCSCSCProvisioning :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCProvisioning = OutboundCrossClusterSearchConnectionStatusCode' "PROVISIONING"

pattern OCCSCSCRejected :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCRejected = OutboundCrossClusterSearchConnectionStatusCode' "REJECTED"

pattern OCCSCSCValidating :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCValidating = OutboundCrossClusterSearchConnectionStatusCode' "VALIDATING"

pattern OCCSCSCValidationFailed :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCValidationFailed = OutboundCrossClusterSearchConnectionStatusCode' "VALIDATION_FAILED"

{-# COMPLETE
  OCCSCSCActive,
  OCCSCSCDeleted,
  OCCSCSCDeleting,
  OCCSCSCPendingAcceptance,
  OCCSCSCProvisioning,
  OCCSCSCRejected,
  OCCSCSCValidating,
  OCCSCSCValidationFailed,
  OutboundCrossClusterSearchConnectionStatusCode'
  #-}
