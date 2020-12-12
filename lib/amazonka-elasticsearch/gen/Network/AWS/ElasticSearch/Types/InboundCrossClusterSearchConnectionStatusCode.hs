{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
  ( InboundCrossClusterSearchConnectionStatusCode
      ( InboundCrossClusterSearchConnectionStatusCode',
        Approved,
        Deleted,
        Deleting,
        PendingAcceptance,
        Rejected,
        Rejecting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InboundCrossClusterSearchConnectionStatusCode = InboundCrossClusterSearchConnectionStatusCode' Lude.Text
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

pattern Approved :: InboundCrossClusterSearchConnectionStatusCode
pattern Approved = InboundCrossClusterSearchConnectionStatusCode' "APPROVED"

pattern Deleted :: InboundCrossClusterSearchConnectionStatusCode
pattern Deleted = InboundCrossClusterSearchConnectionStatusCode' "DELETED"

pattern Deleting :: InboundCrossClusterSearchConnectionStatusCode
pattern Deleting = InboundCrossClusterSearchConnectionStatusCode' "DELETING"

pattern PendingAcceptance :: InboundCrossClusterSearchConnectionStatusCode
pattern PendingAcceptance = InboundCrossClusterSearchConnectionStatusCode' "PENDING_ACCEPTANCE"

pattern Rejected :: InboundCrossClusterSearchConnectionStatusCode
pattern Rejected = InboundCrossClusterSearchConnectionStatusCode' "REJECTED"

pattern Rejecting :: InboundCrossClusterSearchConnectionStatusCode
pattern Rejecting = InboundCrossClusterSearchConnectionStatusCode' "REJECTING"

{-# COMPLETE
  Approved,
  Deleted,
  Deleting,
  PendingAcceptance,
  Rejected,
  Rejecting,
  InboundCrossClusterSearchConnectionStatusCode'
  #-}
