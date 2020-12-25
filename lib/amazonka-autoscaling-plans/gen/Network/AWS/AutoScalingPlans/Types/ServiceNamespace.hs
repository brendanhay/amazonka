{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ServiceNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ServiceNamespace
  ( ServiceNamespace
      ( ServiceNamespace',
        ServiceNamespaceAutoscaling,
        ServiceNamespaceEcs,
        ServiceNamespaceEC2,
        ServiceNamespaceRds,
        ServiceNamespaceDynamodb,
        fromServiceNamespace
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ServiceNamespace = ServiceNamespace'
  { fromServiceNamespace ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ServiceNamespaceAutoscaling :: ServiceNamespace
pattern ServiceNamespaceAutoscaling = ServiceNamespace' "autoscaling"

pattern ServiceNamespaceEcs :: ServiceNamespace
pattern ServiceNamespaceEcs = ServiceNamespace' "ecs"

pattern ServiceNamespaceEC2 :: ServiceNamespace
pattern ServiceNamespaceEC2 = ServiceNamespace' "ec2"

pattern ServiceNamespaceRds :: ServiceNamespace
pattern ServiceNamespaceRds = ServiceNamespace' "rds"

pattern ServiceNamespaceDynamodb :: ServiceNamespace
pattern ServiceNamespaceDynamodb = ServiceNamespace' "dynamodb"

{-# COMPLETE
  ServiceNamespaceAutoscaling,
  ServiceNamespaceEcs,
  ServiceNamespaceEC2,
  ServiceNamespaceRds,
  ServiceNamespaceDynamodb,
  ServiceNamespace'
  #-}
