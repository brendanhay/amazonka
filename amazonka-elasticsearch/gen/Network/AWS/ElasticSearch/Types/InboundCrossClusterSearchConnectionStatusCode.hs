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
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
  ( InboundCrossClusterSearchConnectionStatusCode
      ( ..,
        InboundCrossClusterSearchConnectionStatusCode_APPROVED,
        InboundCrossClusterSearchConnectionStatusCode_DELETED,
        InboundCrossClusterSearchConnectionStatusCode_DELETING,
        InboundCrossClusterSearchConnectionStatusCode_PENDING_ACCEPTANCE,
        InboundCrossClusterSearchConnectionStatusCode_REJECTED,
        InboundCrossClusterSearchConnectionStatusCode_REJECTING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InboundCrossClusterSearchConnectionStatusCode = InboundCrossClusterSearchConnectionStatusCode'
  { fromInboundCrossClusterSearchConnectionStatusCode ::
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

pattern InboundCrossClusterSearchConnectionStatusCode_APPROVED :: InboundCrossClusterSearchConnectionStatusCode
pattern InboundCrossClusterSearchConnectionStatusCode_APPROVED = InboundCrossClusterSearchConnectionStatusCode' "APPROVED"

pattern InboundCrossClusterSearchConnectionStatusCode_DELETED :: InboundCrossClusterSearchConnectionStatusCode
pattern InboundCrossClusterSearchConnectionStatusCode_DELETED = InboundCrossClusterSearchConnectionStatusCode' "DELETED"

pattern InboundCrossClusterSearchConnectionStatusCode_DELETING :: InboundCrossClusterSearchConnectionStatusCode
pattern InboundCrossClusterSearchConnectionStatusCode_DELETING = InboundCrossClusterSearchConnectionStatusCode' "DELETING"

pattern InboundCrossClusterSearchConnectionStatusCode_PENDING_ACCEPTANCE :: InboundCrossClusterSearchConnectionStatusCode
pattern InboundCrossClusterSearchConnectionStatusCode_PENDING_ACCEPTANCE = InboundCrossClusterSearchConnectionStatusCode' "PENDING_ACCEPTANCE"

pattern InboundCrossClusterSearchConnectionStatusCode_REJECTED :: InboundCrossClusterSearchConnectionStatusCode
pattern InboundCrossClusterSearchConnectionStatusCode_REJECTED = InboundCrossClusterSearchConnectionStatusCode' "REJECTED"

pattern InboundCrossClusterSearchConnectionStatusCode_REJECTING :: InboundCrossClusterSearchConnectionStatusCode
pattern InboundCrossClusterSearchConnectionStatusCode_REJECTING = InboundCrossClusterSearchConnectionStatusCode' "REJECTING"

{-# COMPLETE
  InboundCrossClusterSearchConnectionStatusCode_APPROVED,
  InboundCrossClusterSearchConnectionStatusCode_DELETED,
  InboundCrossClusterSearchConnectionStatusCode_DELETING,
  InboundCrossClusterSearchConnectionStatusCode_PENDING_ACCEPTANCE,
  InboundCrossClusterSearchConnectionStatusCode_REJECTED,
  InboundCrossClusterSearchConnectionStatusCode_REJECTING,
  InboundCrossClusterSearchConnectionStatusCode'
  #-}
