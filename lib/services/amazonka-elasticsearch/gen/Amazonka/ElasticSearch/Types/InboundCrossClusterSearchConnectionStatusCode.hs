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
-- Module      : Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InboundCrossClusterSearchConnectionStatusCode = InboundCrossClusterSearchConnectionStatusCode'
  { fromInboundCrossClusterSearchConnectionStatusCode ::
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
