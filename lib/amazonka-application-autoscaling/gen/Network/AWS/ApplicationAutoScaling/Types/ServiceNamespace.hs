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
        Ecs,
        Elasticmapreduce,
        EC2,
        Appstream,
        Dynamodb,
        RDS,
        Sagemaker,
        CustomResource,
        Comprehend,
        Lambda,
        Cassandra,
        Kafka
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

pattern Ecs :: ServiceNamespace
pattern Ecs = ServiceNamespace' "ecs"

pattern Elasticmapreduce :: ServiceNamespace
pattern Elasticmapreduce = ServiceNamespace' "elasticmapreduce"

pattern EC2 :: ServiceNamespace
pattern EC2 = ServiceNamespace' "ec2"

pattern Appstream :: ServiceNamespace
pattern Appstream = ServiceNamespace' "appstream"

pattern Dynamodb :: ServiceNamespace
pattern Dynamodb = ServiceNamespace' "dynamodb"

pattern RDS :: ServiceNamespace
pattern RDS = ServiceNamespace' "rds"

pattern Sagemaker :: ServiceNamespace
pattern Sagemaker = ServiceNamespace' "sagemaker"

pattern CustomResource :: ServiceNamespace
pattern CustomResource = ServiceNamespace' "custom-resource"

pattern Comprehend :: ServiceNamespace
pattern Comprehend = ServiceNamespace' "comprehend"

pattern Lambda :: ServiceNamespace
pattern Lambda = ServiceNamespace' "lambda"

pattern Cassandra :: ServiceNamespace
pattern Cassandra = ServiceNamespace' "cassandra"

pattern Kafka :: ServiceNamespace
pattern Kafka = ServiceNamespace' "kafka"

{-# COMPLETE
  Ecs,
  Elasticmapreduce,
  EC2,
  Appstream,
  Dynamodb,
  RDS,
  Sagemaker,
  CustomResource,
  Comprehend,
  Lambda,
  Cassandra,
  Kafka,
  ServiceNamespace'
  #-}
