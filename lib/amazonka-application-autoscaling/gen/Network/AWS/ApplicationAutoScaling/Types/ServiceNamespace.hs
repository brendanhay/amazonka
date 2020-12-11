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
        Appstream,
        Cassandra,
        Comprehend,
        CustomResource,
        Dynamodb,
        EC2,
        Ecs,
        Elasticmapreduce,
        Kafka,
        Lambda,
        RDS,
        Sagemaker
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ServiceNamespace = ServiceNamespace' Lude.Text
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

pattern Appstream :: ServiceNamespace
pattern Appstream = ServiceNamespace' "appstream"

pattern Cassandra :: ServiceNamespace
pattern Cassandra = ServiceNamespace' "cassandra"

pattern Comprehend :: ServiceNamespace
pattern Comprehend = ServiceNamespace' "comprehend"

pattern CustomResource :: ServiceNamespace
pattern CustomResource = ServiceNamespace' "custom-resource"

pattern Dynamodb :: ServiceNamespace
pattern Dynamodb = ServiceNamespace' "dynamodb"

pattern EC2 :: ServiceNamespace
pattern EC2 = ServiceNamespace' "ec2"

pattern Ecs :: ServiceNamespace
pattern Ecs = ServiceNamespace' "ecs"

pattern Elasticmapreduce :: ServiceNamespace
pattern Elasticmapreduce = ServiceNamespace' "elasticmapreduce"

pattern Kafka :: ServiceNamespace
pattern Kafka = ServiceNamespace' "kafka"

pattern Lambda :: ServiceNamespace
pattern Lambda = ServiceNamespace' "lambda"

pattern RDS :: ServiceNamespace
pattern RDS = ServiceNamespace' "rds"

pattern Sagemaker :: ServiceNamespace
pattern Sagemaker = ServiceNamespace' "sagemaker"

{-# COMPLETE
  Appstream,
  Cassandra,
  Comprehend,
  CustomResource,
  Dynamodb,
  EC2,
  Ecs,
  Elasticmapreduce,
  Kafka,
  Lambda,
  RDS,
  Sagemaker,
  ServiceNamespace'
  #-}
