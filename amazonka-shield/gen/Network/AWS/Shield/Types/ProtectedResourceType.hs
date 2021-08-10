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
-- Module      : Network.AWS.Shield.Types.ProtectedResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.ProtectedResourceType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProtectedResourceType = ProtectedResourceType'
  { fromProtectedResourceType ::
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
