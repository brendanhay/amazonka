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
-- Module      : Amazonka.Shield.Types.ProtectedResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.ProtectedResourceType
  ( ProtectedResourceType
      ( ..,
        ProtectedResourceType_APPLICATION_LOAD_BALANCER,
        ProtectedResourceType_CLASSIC_LOAD_BALANCER,
        ProtectedResourceType_CLOUDFRONT_DISTRIBUTION,
        ProtectedResourceType_ELASTIC_IP_ALLOCATION,
        ProtectedResourceType_GLOBAL_ACCELERATOR,
        ProtectedResourceType_ROUTE_53_HOSTED_ZONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProtectedResourceType = ProtectedResourceType'
  { fromProtectedResourceType ::
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

pattern ProtectedResourceType_APPLICATION_LOAD_BALANCER :: ProtectedResourceType
pattern ProtectedResourceType_APPLICATION_LOAD_BALANCER = ProtectedResourceType' "APPLICATION_LOAD_BALANCER"

pattern ProtectedResourceType_CLASSIC_LOAD_BALANCER :: ProtectedResourceType
pattern ProtectedResourceType_CLASSIC_LOAD_BALANCER = ProtectedResourceType' "CLASSIC_LOAD_BALANCER"

pattern ProtectedResourceType_CLOUDFRONT_DISTRIBUTION :: ProtectedResourceType
pattern ProtectedResourceType_CLOUDFRONT_DISTRIBUTION = ProtectedResourceType' "CLOUDFRONT_DISTRIBUTION"

pattern ProtectedResourceType_ELASTIC_IP_ALLOCATION :: ProtectedResourceType
pattern ProtectedResourceType_ELASTIC_IP_ALLOCATION = ProtectedResourceType' "ELASTIC_IP_ALLOCATION"

pattern ProtectedResourceType_GLOBAL_ACCELERATOR :: ProtectedResourceType
pattern ProtectedResourceType_GLOBAL_ACCELERATOR = ProtectedResourceType' "GLOBAL_ACCELERATOR"

pattern ProtectedResourceType_ROUTE_53_HOSTED_ZONE :: ProtectedResourceType
pattern ProtectedResourceType_ROUTE_53_HOSTED_ZONE = ProtectedResourceType' "ROUTE_53_HOSTED_ZONE"

{-# COMPLETE
  ProtectedResourceType_APPLICATION_LOAD_BALANCER,
  ProtectedResourceType_CLASSIC_LOAD_BALANCER,
  ProtectedResourceType_CLOUDFRONT_DISTRIBUTION,
  ProtectedResourceType_ELASTIC_IP_ALLOCATION,
  ProtectedResourceType_GLOBAL_ACCELERATOR,
  ProtectedResourceType_ROUTE_53_HOSTED_ZONE,
  ProtectedResourceType'
  #-}
