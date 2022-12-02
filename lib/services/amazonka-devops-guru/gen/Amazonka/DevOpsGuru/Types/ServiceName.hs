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
-- Module      : Amazonka.DevOpsGuru.Types.ServiceName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ServiceName
  ( ServiceName
      ( ..,
        ServiceName_API_GATEWAY,
        ServiceName_APPLICATION_ELB,
        ServiceName_AUTO_SCALING_GROUP,
        ServiceName_CLOUD_FRONT,
        ServiceName_DYNAMO_DB,
        ServiceName_EC2,
        ServiceName_ECS,
        ServiceName_EKS,
        ServiceName_ELASTIC_BEANSTALK,
        ServiceName_ELASTI_CACHE,
        ServiceName_ELB,
        ServiceName_ES,
        ServiceName_KINESIS,
        ServiceName_LAMBDA,
        ServiceName_NAT_GATEWAY,
        ServiceName_NETWORK_ELB,
        ServiceName_RDS,
        ServiceName_REDSHIFT,
        ServiceName_ROUTE_53,
        ServiceName_S3,
        ServiceName_SAGE_MAKER,
        ServiceName_SNS,
        ServiceName_SQS,
        ServiceName_STEP_FUNCTIONS,
        ServiceName_SWF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceName = ServiceName'
  { fromServiceName ::
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

pattern ServiceName_API_GATEWAY :: ServiceName
pattern ServiceName_API_GATEWAY = ServiceName' "API_GATEWAY"

pattern ServiceName_APPLICATION_ELB :: ServiceName
pattern ServiceName_APPLICATION_ELB = ServiceName' "APPLICATION_ELB"

pattern ServiceName_AUTO_SCALING_GROUP :: ServiceName
pattern ServiceName_AUTO_SCALING_GROUP = ServiceName' "AUTO_SCALING_GROUP"

pattern ServiceName_CLOUD_FRONT :: ServiceName
pattern ServiceName_CLOUD_FRONT = ServiceName' "CLOUD_FRONT"

pattern ServiceName_DYNAMO_DB :: ServiceName
pattern ServiceName_DYNAMO_DB = ServiceName' "DYNAMO_DB"

pattern ServiceName_EC2 :: ServiceName
pattern ServiceName_EC2 = ServiceName' "EC2"

pattern ServiceName_ECS :: ServiceName
pattern ServiceName_ECS = ServiceName' "ECS"

pattern ServiceName_EKS :: ServiceName
pattern ServiceName_EKS = ServiceName' "EKS"

pattern ServiceName_ELASTIC_BEANSTALK :: ServiceName
pattern ServiceName_ELASTIC_BEANSTALK = ServiceName' "ELASTIC_BEANSTALK"

pattern ServiceName_ELASTI_CACHE :: ServiceName
pattern ServiceName_ELASTI_CACHE = ServiceName' "ELASTI_CACHE"

pattern ServiceName_ELB :: ServiceName
pattern ServiceName_ELB = ServiceName' "ELB"

pattern ServiceName_ES :: ServiceName
pattern ServiceName_ES = ServiceName' "ES"

pattern ServiceName_KINESIS :: ServiceName
pattern ServiceName_KINESIS = ServiceName' "KINESIS"

pattern ServiceName_LAMBDA :: ServiceName
pattern ServiceName_LAMBDA = ServiceName' "LAMBDA"

pattern ServiceName_NAT_GATEWAY :: ServiceName
pattern ServiceName_NAT_GATEWAY = ServiceName' "NAT_GATEWAY"

pattern ServiceName_NETWORK_ELB :: ServiceName
pattern ServiceName_NETWORK_ELB = ServiceName' "NETWORK_ELB"

pattern ServiceName_RDS :: ServiceName
pattern ServiceName_RDS = ServiceName' "RDS"

pattern ServiceName_REDSHIFT :: ServiceName
pattern ServiceName_REDSHIFT = ServiceName' "REDSHIFT"

pattern ServiceName_ROUTE_53 :: ServiceName
pattern ServiceName_ROUTE_53 = ServiceName' "ROUTE_53"

pattern ServiceName_S3 :: ServiceName
pattern ServiceName_S3 = ServiceName' "S3"

pattern ServiceName_SAGE_MAKER :: ServiceName
pattern ServiceName_SAGE_MAKER = ServiceName' "SAGE_MAKER"

pattern ServiceName_SNS :: ServiceName
pattern ServiceName_SNS = ServiceName' "SNS"

pattern ServiceName_SQS :: ServiceName
pattern ServiceName_SQS = ServiceName' "SQS"

pattern ServiceName_STEP_FUNCTIONS :: ServiceName
pattern ServiceName_STEP_FUNCTIONS = ServiceName' "STEP_FUNCTIONS"

pattern ServiceName_SWF :: ServiceName
pattern ServiceName_SWF = ServiceName' "SWF"

{-# COMPLETE
  ServiceName_API_GATEWAY,
  ServiceName_APPLICATION_ELB,
  ServiceName_AUTO_SCALING_GROUP,
  ServiceName_CLOUD_FRONT,
  ServiceName_DYNAMO_DB,
  ServiceName_EC2,
  ServiceName_ECS,
  ServiceName_EKS,
  ServiceName_ELASTIC_BEANSTALK,
  ServiceName_ELASTI_CACHE,
  ServiceName_ELB,
  ServiceName_ES,
  ServiceName_KINESIS,
  ServiceName_LAMBDA,
  ServiceName_NAT_GATEWAY,
  ServiceName_NETWORK_ELB,
  ServiceName_RDS,
  ServiceName_REDSHIFT,
  ServiceName_ROUTE_53,
  ServiceName_S3,
  ServiceName_SAGE_MAKER,
  ServiceName_SNS,
  ServiceName_SQS,
  ServiceName_STEP_FUNCTIONS,
  ServiceName_SWF,
  ServiceName'
  #-}
