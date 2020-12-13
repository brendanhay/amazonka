{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        OCCSCSCPendingAcceptance,
        OCCSCSCValidating,
        OCCSCSCValidationFailed,
        OCCSCSCProvisioning,
        OCCSCSCActive,
        OCCSCSCRejected,
        OCCSCSCDeleting,
        OCCSCSCDeleted
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

pattern OCCSCSCPendingAcceptance :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCPendingAcceptance = OutboundCrossClusterSearchConnectionStatusCode' "PENDING_ACCEPTANCE"

pattern OCCSCSCValidating :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCValidating = OutboundCrossClusterSearchConnectionStatusCode' "VALIDATING"

pattern OCCSCSCValidationFailed :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCValidationFailed = OutboundCrossClusterSearchConnectionStatusCode' "VALIDATION_FAILED"

pattern OCCSCSCProvisioning :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCProvisioning = OutboundCrossClusterSearchConnectionStatusCode' "PROVISIONING"

pattern OCCSCSCActive :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCActive = OutboundCrossClusterSearchConnectionStatusCode' "ACTIVE"

pattern OCCSCSCRejected :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCRejected = OutboundCrossClusterSearchConnectionStatusCode' "REJECTED"

pattern OCCSCSCDeleting :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCDeleting = OutboundCrossClusterSearchConnectionStatusCode' "DELETING"

pattern OCCSCSCDeleted :: OutboundCrossClusterSearchConnectionStatusCode
pattern OCCSCSCDeleted = OutboundCrossClusterSearchConnectionStatusCode' "DELETED"

{-# COMPLETE
  OCCSCSCPendingAcceptance,
  OCCSCSCValidating,
  OCCSCSCValidationFailed,
  OCCSCSCProvisioning,
  OCCSCSCActive,
  OCCSCSCRejected,
  OCCSCSCDeleting,
  OCCSCSCDeleted,
  OutboundCrossClusterSearchConnectionStatusCode'
  #-}
