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
-- Module      : Amazonka.ApplicationAutoScaling.Types.ServiceNamespace
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationAutoScaling.Types.ServiceNamespace
  ( ServiceNamespace
      ( ..,
        ServiceNamespace_Appstream,
        ServiceNamespace_Cassandra,
        ServiceNamespace_Comprehend,
        ServiceNamespace_Custom_resource,
        ServiceNamespace_Dynamodb,
        ServiceNamespace_Ec2,
        ServiceNamespace_Ecs,
        ServiceNamespace_Elasticache,
        ServiceNamespace_Elasticmapreduce,
        ServiceNamespace_Kafka,
        ServiceNamespace_Lambda,
        ServiceNamespace_Neptune,
        ServiceNamespace_Rds,
        ServiceNamespace_Sagemaker
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ServiceNamespace = ServiceNamespace'
  { fromServiceNamespace ::
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

pattern ServiceNamespace_Appstream :: ServiceNamespace
pattern ServiceNamespace_Appstream = ServiceNamespace' "appstream"

pattern ServiceNamespace_Cassandra :: ServiceNamespace
pattern ServiceNamespace_Cassandra = ServiceNamespace' "cassandra"

pattern ServiceNamespace_Comprehend :: ServiceNamespace
pattern ServiceNamespace_Comprehend = ServiceNamespace' "comprehend"

pattern ServiceNamespace_Custom_resource :: ServiceNamespace
pattern ServiceNamespace_Custom_resource = ServiceNamespace' "custom-resource"

pattern ServiceNamespace_Dynamodb :: ServiceNamespace
pattern ServiceNamespace_Dynamodb = ServiceNamespace' "dynamodb"

pattern ServiceNamespace_Ec2 :: ServiceNamespace
pattern ServiceNamespace_Ec2 = ServiceNamespace' "ec2"

pattern ServiceNamespace_Ecs :: ServiceNamespace
pattern ServiceNamespace_Ecs = ServiceNamespace' "ecs"

pattern ServiceNamespace_Elasticache :: ServiceNamespace
pattern ServiceNamespace_Elasticache = ServiceNamespace' "elasticache"

pattern ServiceNamespace_Elasticmapreduce :: ServiceNamespace
pattern ServiceNamespace_Elasticmapreduce = ServiceNamespace' "elasticmapreduce"

pattern ServiceNamespace_Kafka :: ServiceNamespace
pattern ServiceNamespace_Kafka = ServiceNamespace' "kafka"

pattern ServiceNamespace_Lambda :: ServiceNamespace
pattern ServiceNamespace_Lambda = ServiceNamespace' "lambda"

pattern ServiceNamespace_Neptune :: ServiceNamespace
pattern ServiceNamespace_Neptune = ServiceNamespace' "neptune"

pattern ServiceNamespace_Rds :: ServiceNamespace
pattern ServiceNamespace_Rds = ServiceNamespace' "rds"

pattern ServiceNamespace_Sagemaker :: ServiceNamespace
pattern ServiceNamespace_Sagemaker = ServiceNamespace' "sagemaker"

{-# COMPLETE
  ServiceNamespace_Appstream,
  ServiceNamespace_Cassandra,
  ServiceNamespace_Comprehend,
  ServiceNamespace_Custom_resource,
  ServiceNamespace_Dynamodb,
  ServiceNamespace_Ec2,
  ServiceNamespace_Ecs,
  ServiceNamespace_Elasticache,
  ServiceNamespace_Elasticmapreduce,
  ServiceNamespace_Kafka,
  ServiceNamespace_Lambda,
  ServiceNamespace_Neptune,
  ServiceNamespace_Rds,
  ServiceNamespace_Sagemaker,
  ServiceNamespace'
  #-}
