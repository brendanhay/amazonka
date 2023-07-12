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
-- Module      : Amazonka.DevOpsGuru.Types.ResourceTypeFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ResourceTypeFilter
  ( ResourceTypeFilter
      ( ..,
        ResourceTypeFilter_CLOUDFRONT_DISTRIBUTION,
        ResourceTypeFilter_DYNAMODB_TABLE,
        ResourceTypeFilter_EC2_NAT_GATEWAY,
        ResourceTypeFilter_ECS_CLUSTER,
        ResourceTypeFilter_ECS_SERVICE,
        ResourceTypeFilter_EKS_CLUSTER,
        ResourceTypeFilter_ELASTICACHE_CACHE_CLUSTER,
        ResourceTypeFilter_ELASTICSEARCH_DOMAIN,
        ResourceTypeFilter_ELASTIC_BEANSTALK_ENVIRONMENT,
        ResourceTypeFilter_ELASTIC_LOAD_BALANCER_LOAD_BALANCER,
        ResourceTypeFilter_ELASTIC_LOAD_BALANCING_V2_LOAD_BALANCER,
        ResourceTypeFilter_ELASTIC_LOAD_BALANCING_V2_TARGET_GROUP,
        ResourceTypeFilter_KINESIS_STREAM,
        ResourceTypeFilter_LAMBDA_FUNCTION,
        ResourceTypeFilter_LOG_GROUPS,
        ResourceTypeFilter_OPEN_SEARCH_SERVICE_DOMAIN,
        ResourceTypeFilter_RDS_DB_CLUSTER,
        ResourceTypeFilter_RDS_DB_INSTANCE,
        ResourceTypeFilter_REDSHIFT_CLUSTER,
        ResourceTypeFilter_ROUTE53_HEALTH_CHECK,
        ResourceTypeFilter_ROUTE53_HOSTED_ZONE,
        ResourceTypeFilter_S3_BUCKET,
        ResourceTypeFilter_SAGEMAKER_ENDPOINT,
        ResourceTypeFilter_SNS_TOPIC,
        ResourceTypeFilter_SQS_QUEUE,
        ResourceTypeFilter_STEP_FUNCTIONS_ACTIVITY,
        ResourceTypeFilter_STEP_FUNCTIONS_STATE_MACHINE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceTypeFilter = ResourceTypeFilter'
  { fromResourceTypeFilter ::
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

pattern ResourceTypeFilter_CLOUDFRONT_DISTRIBUTION :: ResourceTypeFilter
pattern ResourceTypeFilter_CLOUDFRONT_DISTRIBUTION = ResourceTypeFilter' "CLOUDFRONT_DISTRIBUTION"

pattern ResourceTypeFilter_DYNAMODB_TABLE :: ResourceTypeFilter
pattern ResourceTypeFilter_DYNAMODB_TABLE = ResourceTypeFilter' "DYNAMODB_TABLE"

pattern ResourceTypeFilter_EC2_NAT_GATEWAY :: ResourceTypeFilter
pattern ResourceTypeFilter_EC2_NAT_GATEWAY = ResourceTypeFilter' "EC2_NAT_GATEWAY"

pattern ResourceTypeFilter_ECS_CLUSTER :: ResourceTypeFilter
pattern ResourceTypeFilter_ECS_CLUSTER = ResourceTypeFilter' "ECS_CLUSTER"

pattern ResourceTypeFilter_ECS_SERVICE :: ResourceTypeFilter
pattern ResourceTypeFilter_ECS_SERVICE = ResourceTypeFilter' "ECS_SERVICE"

pattern ResourceTypeFilter_EKS_CLUSTER :: ResourceTypeFilter
pattern ResourceTypeFilter_EKS_CLUSTER = ResourceTypeFilter' "EKS_CLUSTER"

pattern ResourceTypeFilter_ELASTICACHE_CACHE_CLUSTER :: ResourceTypeFilter
pattern ResourceTypeFilter_ELASTICACHE_CACHE_CLUSTER = ResourceTypeFilter' "ELASTICACHE_CACHE_CLUSTER"

pattern ResourceTypeFilter_ELASTICSEARCH_DOMAIN :: ResourceTypeFilter
pattern ResourceTypeFilter_ELASTICSEARCH_DOMAIN = ResourceTypeFilter' "ELASTICSEARCH_DOMAIN"

pattern ResourceTypeFilter_ELASTIC_BEANSTALK_ENVIRONMENT :: ResourceTypeFilter
pattern ResourceTypeFilter_ELASTIC_BEANSTALK_ENVIRONMENT = ResourceTypeFilter' "ELASTIC_BEANSTALK_ENVIRONMENT"

pattern ResourceTypeFilter_ELASTIC_LOAD_BALANCER_LOAD_BALANCER :: ResourceTypeFilter
pattern ResourceTypeFilter_ELASTIC_LOAD_BALANCER_LOAD_BALANCER = ResourceTypeFilter' "ELASTIC_LOAD_BALANCER_LOAD_BALANCER"

pattern ResourceTypeFilter_ELASTIC_LOAD_BALANCING_V2_LOAD_BALANCER :: ResourceTypeFilter
pattern ResourceTypeFilter_ELASTIC_LOAD_BALANCING_V2_LOAD_BALANCER = ResourceTypeFilter' "ELASTIC_LOAD_BALANCING_V2_LOAD_BALANCER"

pattern ResourceTypeFilter_ELASTIC_LOAD_BALANCING_V2_TARGET_GROUP :: ResourceTypeFilter
pattern ResourceTypeFilter_ELASTIC_LOAD_BALANCING_V2_TARGET_GROUP = ResourceTypeFilter' "ELASTIC_LOAD_BALANCING_V2_TARGET_GROUP"

pattern ResourceTypeFilter_KINESIS_STREAM :: ResourceTypeFilter
pattern ResourceTypeFilter_KINESIS_STREAM = ResourceTypeFilter' "KINESIS_STREAM"

pattern ResourceTypeFilter_LAMBDA_FUNCTION :: ResourceTypeFilter
pattern ResourceTypeFilter_LAMBDA_FUNCTION = ResourceTypeFilter' "LAMBDA_FUNCTION"

pattern ResourceTypeFilter_LOG_GROUPS :: ResourceTypeFilter
pattern ResourceTypeFilter_LOG_GROUPS = ResourceTypeFilter' "LOG_GROUPS"

pattern ResourceTypeFilter_OPEN_SEARCH_SERVICE_DOMAIN :: ResourceTypeFilter
pattern ResourceTypeFilter_OPEN_SEARCH_SERVICE_DOMAIN = ResourceTypeFilter' "OPEN_SEARCH_SERVICE_DOMAIN"

pattern ResourceTypeFilter_RDS_DB_CLUSTER :: ResourceTypeFilter
pattern ResourceTypeFilter_RDS_DB_CLUSTER = ResourceTypeFilter' "RDS_DB_CLUSTER"

pattern ResourceTypeFilter_RDS_DB_INSTANCE :: ResourceTypeFilter
pattern ResourceTypeFilter_RDS_DB_INSTANCE = ResourceTypeFilter' "RDS_DB_INSTANCE"

pattern ResourceTypeFilter_REDSHIFT_CLUSTER :: ResourceTypeFilter
pattern ResourceTypeFilter_REDSHIFT_CLUSTER = ResourceTypeFilter' "REDSHIFT_CLUSTER"

pattern ResourceTypeFilter_ROUTE53_HEALTH_CHECK :: ResourceTypeFilter
pattern ResourceTypeFilter_ROUTE53_HEALTH_CHECK = ResourceTypeFilter' "ROUTE53_HEALTH_CHECK"

pattern ResourceTypeFilter_ROUTE53_HOSTED_ZONE :: ResourceTypeFilter
pattern ResourceTypeFilter_ROUTE53_HOSTED_ZONE = ResourceTypeFilter' "ROUTE53_HOSTED_ZONE"

pattern ResourceTypeFilter_S3_BUCKET :: ResourceTypeFilter
pattern ResourceTypeFilter_S3_BUCKET = ResourceTypeFilter' "S3_BUCKET"

pattern ResourceTypeFilter_SAGEMAKER_ENDPOINT :: ResourceTypeFilter
pattern ResourceTypeFilter_SAGEMAKER_ENDPOINT = ResourceTypeFilter' "SAGEMAKER_ENDPOINT"

pattern ResourceTypeFilter_SNS_TOPIC :: ResourceTypeFilter
pattern ResourceTypeFilter_SNS_TOPIC = ResourceTypeFilter' "SNS_TOPIC"

pattern ResourceTypeFilter_SQS_QUEUE :: ResourceTypeFilter
pattern ResourceTypeFilter_SQS_QUEUE = ResourceTypeFilter' "SQS_QUEUE"

pattern ResourceTypeFilter_STEP_FUNCTIONS_ACTIVITY :: ResourceTypeFilter
pattern ResourceTypeFilter_STEP_FUNCTIONS_ACTIVITY = ResourceTypeFilter' "STEP_FUNCTIONS_ACTIVITY"

pattern ResourceTypeFilter_STEP_FUNCTIONS_STATE_MACHINE :: ResourceTypeFilter
pattern ResourceTypeFilter_STEP_FUNCTIONS_STATE_MACHINE = ResourceTypeFilter' "STEP_FUNCTIONS_STATE_MACHINE"

{-# COMPLETE
  ResourceTypeFilter_CLOUDFRONT_DISTRIBUTION,
  ResourceTypeFilter_DYNAMODB_TABLE,
  ResourceTypeFilter_EC2_NAT_GATEWAY,
  ResourceTypeFilter_ECS_CLUSTER,
  ResourceTypeFilter_ECS_SERVICE,
  ResourceTypeFilter_EKS_CLUSTER,
  ResourceTypeFilter_ELASTICACHE_CACHE_CLUSTER,
  ResourceTypeFilter_ELASTICSEARCH_DOMAIN,
  ResourceTypeFilter_ELASTIC_BEANSTALK_ENVIRONMENT,
  ResourceTypeFilter_ELASTIC_LOAD_BALANCER_LOAD_BALANCER,
  ResourceTypeFilter_ELASTIC_LOAD_BALANCING_V2_LOAD_BALANCER,
  ResourceTypeFilter_ELASTIC_LOAD_BALANCING_V2_TARGET_GROUP,
  ResourceTypeFilter_KINESIS_STREAM,
  ResourceTypeFilter_LAMBDA_FUNCTION,
  ResourceTypeFilter_LOG_GROUPS,
  ResourceTypeFilter_OPEN_SEARCH_SERVICE_DOMAIN,
  ResourceTypeFilter_RDS_DB_CLUSTER,
  ResourceTypeFilter_RDS_DB_INSTANCE,
  ResourceTypeFilter_REDSHIFT_CLUSTER,
  ResourceTypeFilter_ROUTE53_HEALTH_CHECK,
  ResourceTypeFilter_ROUTE53_HOSTED_ZONE,
  ResourceTypeFilter_S3_BUCKET,
  ResourceTypeFilter_SAGEMAKER_ENDPOINT,
  ResourceTypeFilter_SNS_TOPIC,
  ResourceTypeFilter_SQS_QUEUE,
  ResourceTypeFilter_STEP_FUNCTIONS_ACTIVITY,
  ResourceTypeFilter_STEP_FUNCTIONS_STATE_MACHINE,
  ResourceTypeFilter'
  #-}
