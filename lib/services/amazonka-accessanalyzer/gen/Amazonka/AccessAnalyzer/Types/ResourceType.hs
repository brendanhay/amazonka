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
-- Module      : Amazonka.AccessAnalyzer.Types.ResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_AWS__EC2__Snapshot,
        ResourceType_AWS__ECR__Repository,
        ResourceType_AWS__EFS__FileSystem,
        ResourceType_AWS__IAM__Role,
        ResourceType_AWS__KMS__Key,
        ResourceType_AWS__Lambda__Function,
        ResourceType_AWS__Lambda__LayerVersion,
        ResourceType_AWS__RDS__DBClusterSnapshot,
        ResourceType_AWS__RDS__DBSnapshot,
        ResourceType_AWS__S3__Bucket,
        ResourceType_AWS__SNS__Topic,
        ResourceType_AWS__SQS__Queue,
        ResourceType_AWS__SecretsManager__Secret
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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

pattern ResourceType_AWS__EC2__Snapshot :: ResourceType
pattern ResourceType_AWS__EC2__Snapshot = ResourceType' "AWS::EC2::Snapshot"

pattern ResourceType_AWS__ECR__Repository :: ResourceType
pattern ResourceType_AWS__ECR__Repository = ResourceType' "AWS::ECR::Repository"

pattern ResourceType_AWS__EFS__FileSystem :: ResourceType
pattern ResourceType_AWS__EFS__FileSystem = ResourceType' "AWS::EFS::FileSystem"

pattern ResourceType_AWS__IAM__Role :: ResourceType
pattern ResourceType_AWS__IAM__Role = ResourceType' "AWS::IAM::Role"

pattern ResourceType_AWS__KMS__Key :: ResourceType
pattern ResourceType_AWS__KMS__Key = ResourceType' "AWS::KMS::Key"

pattern ResourceType_AWS__Lambda__Function :: ResourceType
pattern ResourceType_AWS__Lambda__Function = ResourceType' "AWS::Lambda::Function"

pattern ResourceType_AWS__Lambda__LayerVersion :: ResourceType
pattern ResourceType_AWS__Lambda__LayerVersion = ResourceType' "AWS::Lambda::LayerVersion"

pattern ResourceType_AWS__RDS__DBClusterSnapshot :: ResourceType
pattern ResourceType_AWS__RDS__DBClusterSnapshot = ResourceType' "AWS::RDS::DBClusterSnapshot"

pattern ResourceType_AWS__RDS__DBSnapshot :: ResourceType
pattern ResourceType_AWS__RDS__DBSnapshot = ResourceType' "AWS::RDS::DBSnapshot"

pattern ResourceType_AWS__S3__Bucket :: ResourceType
pattern ResourceType_AWS__S3__Bucket = ResourceType' "AWS::S3::Bucket"

pattern ResourceType_AWS__SNS__Topic :: ResourceType
pattern ResourceType_AWS__SNS__Topic = ResourceType' "AWS::SNS::Topic"

pattern ResourceType_AWS__SQS__Queue :: ResourceType
pattern ResourceType_AWS__SQS__Queue = ResourceType' "AWS::SQS::Queue"

pattern ResourceType_AWS__SecretsManager__Secret :: ResourceType
pattern ResourceType_AWS__SecretsManager__Secret = ResourceType' "AWS::SecretsManager::Secret"

{-# COMPLETE
  ResourceType_AWS__EC2__Snapshot,
  ResourceType_AWS__ECR__Repository,
  ResourceType_AWS__EFS__FileSystem,
  ResourceType_AWS__IAM__Role,
  ResourceType_AWS__KMS__Key,
  ResourceType_AWS__Lambda__Function,
  ResourceType_AWS__Lambda__LayerVersion,
  ResourceType_AWS__RDS__DBClusterSnapshot,
  ResourceType_AWS__RDS__DBSnapshot,
  ResourceType_AWS__S3__Bucket,
  ResourceType_AWS__SNS__Topic,
  ResourceType_AWS__SQS__Queue,
  ResourceType_AWS__SecretsManager__Secret,
  ResourceType'
  #-}
