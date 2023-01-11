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
-- Module      : Amazonka.MigrationHubStrategy.Types.TargetDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.TargetDestination
  ( TargetDestination
      ( ..,
        TargetDestination_AWS_Elastic_BeanStalk,
        TargetDestination_AWS_Fargate,
        TargetDestination_Amazon_DocumentDB,
        TargetDestination_Amazon_DynamoDB,
        TargetDestination_Amazon_Elastic_Cloud_Compute__EC2_,
        TargetDestination_Amazon_Elastic_Container_Service__ECS_,
        TargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_,
        TargetDestination_Amazon_Relational_Database_Service,
        TargetDestination_Amazon_Relational_Database_Service_on_MySQL,
        TargetDestination_Amazon_Relational_Database_Service_on_PostgreSQL,
        TargetDestination_Aurora_MySQL,
        TargetDestination_Aurora_PostgreSQL,
        TargetDestination_Babelfish_for_Aurora_PostgreSQL,
        TargetDestination_None_specified
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetDestination = TargetDestination'
  { fromTargetDestination ::
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

pattern TargetDestination_AWS_Elastic_BeanStalk :: TargetDestination
pattern TargetDestination_AWS_Elastic_BeanStalk = TargetDestination' "AWS Elastic BeanStalk"

pattern TargetDestination_AWS_Fargate :: TargetDestination
pattern TargetDestination_AWS_Fargate = TargetDestination' "AWS Fargate"

pattern TargetDestination_Amazon_DocumentDB :: TargetDestination
pattern TargetDestination_Amazon_DocumentDB = TargetDestination' "Amazon DocumentDB"

pattern TargetDestination_Amazon_DynamoDB :: TargetDestination
pattern TargetDestination_Amazon_DynamoDB = TargetDestination' "Amazon DynamoDB"

pattern TargetDestination_Amazon_Elastic_Cloud_Compute__EC2_ :: TargetDestination
pattern TargetDestination_Amazon_Elastic_Cloud_Compute__EC2_ = TargetDestination' "Amazon Elastic Cloud Compute (EC2)"

pattern TargetDestination_Amazon_Elastic_Container_Service__ECS_ :: TargetDestination
pattern TargetDestination_Amazon_Elastic_Container_Service__ECS_ = TargetDestination' "Amazon Elastic Container Service (ECS)"

pattern TargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_ :: TargetDestination
pattern TargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_ = TargetDestination' "Amazon Elastic Kubernetes Service (EKS)"

pattern TargetDestination_Amazon_Relational_Database_Service :: TargetDestination
pattern TargetDestination_Amazon_Relational_Database_Service = TargetDestination' "Amazon Relational Database Service"

pattern TargetDestination_Amazon_Relational_Database_Service_on_MySQL :: TargetDestination
pattern TargetDestination_Amazon_Relational_Database_Service_on_MySQL = TargetDestination' "Amazon Relational Database Service on MySQL"

pattern TargetDestination_Amazon_Relational_Database_Service_on_PostgreSQL :: TargetDestination
pattern TargetDestination_Amazon_Relational_Database_Service_on_PostgreSQL = TargetDestination' "Amazon Relational Database Service on PostgreSQL"

pattern TargetDestination_Aurora_MySQL :: TargetDestination
pattern TargetDestination_Aurora_MySQL = TargetDestination' "Aurora MySQL"

pattern TargetDestination_Aurora_PostgreSQL :: TargetDestination
pattern TargetDestination_Aurora_PostgreSQL = TargetDestination' "Aurora PostgreSQL"

pattern TargetDestination_Babelfish_for_Aurora_PostgreSQL :: TargetDestination
pattern TargetDestination_Babelfish_for_Aurora_PostgreSQL = TargetDestination' "Babelfish for Aurora PostgreSQL"

pattern TargetDestination_None_specified :: TargetDestination
pattern TargetDestination_None_specified = TargetDestination' "None specified"

{-# COMPLETE
  TargetDestination_AWS_Elastic_BeanStalk,
  TargetDestination_AWS_Fargate,
  TargetDestination_Amazon_DocumentDB,
  TargetDestination_Amazon_DynamoDB,
  TargetDestination_Amazon_Elastic_Cloud_Compute__EC2_,
  TargetDestination_Amazon_Elastic_Container_Service__ECS_,
  TargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_,
  TargetDestination_Amazon_Relational_Database_Service,
  TargetDestination_Amazon_Relational_Database_Service_on_MySQL,
  TargetDestination_Amazon_Relational_Database_Service_on_PostgreSQL,
  TargetDestination_Aurora_MySQL,
  TargetDestination_Aurora_PostgreSQL,
  TargetDestination_Babelfish_for_Aurora_PostgreSQL,
  TargetDestination_None_specified,
  TargetDestination'
  #-}
