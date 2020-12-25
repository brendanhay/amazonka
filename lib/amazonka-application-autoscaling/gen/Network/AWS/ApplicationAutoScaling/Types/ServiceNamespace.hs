{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApplicationAutoScaling.Types.ServiceNamespace
  ( ServiceNamespace
      ( ServiceNamespace',
        ServiceNamespaceEcs,
        ServiceNamespaceElasticmapreduce,
        ServiceNamespaceEC2,
        ServiceNamespaceAppstream,
        ServiceNamespaceDynamodb,
        ServiceNamespaceRds,
        ServiceNamespaceSagemaker,
        ServiceNamespaceCustomResource,
        ServiceNamespaceComprehend,
        ServiceNamespaceLambda,
        ServiceNamespaceCassandra,
        ServiceNamespaceKafka,
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

pattern ServiceNamespaceEcs :: ServiceNamespace
pattern ServiceNamespaceEcs = ServiceNamespace' "ecs"

pattern ServiceNamespaceElasticmapreduce :: ServiceNamespace
pattern ServiceNamespaceElasticmapreduce = ServiceNamespace' "elasticmapreduce"

pattern ServiceNamespaceEC2 :: ServiceNamespace
pattern ServiceNamespaceEC2 = ServiceNamespace' "ec2"

pattern ServiceNamespaceAppstream :: ServiceNamespace
pattern ServiceNamespaceAppstream = ServiceNamespace' "appstream"

pattern ServiceNamespaceDynamodb :: ServiceNamespace
pattern ServiceNamespaceDynamodb = ServiceNamespace' "dynamodb"

pattern ServiceNamespaceRds :: ServiceNamespace
pattern ServiceNamespaceRds = ServiceNamespace' "rds"

pattern ServiceNamespaceSagemaker :: ServiceNamespace
pattern ServiceNamespaceSagemaker = ServiceNamespace' "sagemaker"

pattern ServiceNamespaceCustomResource :: ServiceNamespace
pattern ServiceNamespaceCustomResource = ServiceNamespace' "custom-resource"

pattern ServiceNamespaceComprehend :: ServiceNamespace
pattern ServiceNamespaceComprehend = ServiceNamespace' "comprehend"

pattern ServiceNamespaceLambda :: ServiceNamespace
pattern ServiceNamespaceLambda = ServiceNamespace' "lambda"

pattern ServiceNamespaceCassandra :: ServiceNamespace
pattern ServiceNamespaceCassandra = ServiceNamespace' "cassandra"

pattern ServiceNamespaceKafka :: ServiceNamespace
pattern ServiceNamespaceKafka = ServiceNamespace' "kafka"

{-# COMPLETE
  ServiceNamespaceEcs,
  ServiceNamespaceElasticmapreduce,
  ServiceNamespaceEC2,
  ServiceNamespaceAppstream,
  ServiceNamespaceDynamodb,
  ServiceNamespaceRds,
  ServiceNamespaceSagemaker,
  ServiceNamespaceCustomResource,
  ServiceNamespaceComprehend,
  ServiceNamespaceLambda,
  ServiceNamespaceCassandra,
  ServiceNamespaceKafka,
  ServiceNamespace'
  #-}
