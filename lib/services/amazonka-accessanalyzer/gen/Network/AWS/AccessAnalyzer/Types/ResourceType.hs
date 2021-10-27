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
-- Module      : Network.AWS.AccessAnalyzer.Types.ResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_AWS__IAM__Role,
        ResourceType_AWS__KMS__Key,
        ResourceType_AWS__Lambda__Function,
        ResourceType_AWS__Lambda__LayerVersion,
        ResourceType_AWS__S3__Bucket,
        ResourceType_AWS__SQS__Queue,
        ResourceType_AWS__SecretsManager__Secret
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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

pattern ResourceType_AWS__IAM__Role :: ResourceType
pattern ResourceType_AWS__IAM__Role = ResourceType' "AWS::IAM::Role"

pattern ResourceType_AWS__KMS__Key :: ResourceType
pattern ResourceType_AWS__KMS__Key = ResourceType' "AWS::KMS::Key"

pattern ResourceType_AWS__Lambda__Function :: ResourceType
pattern ResourceType_AWS__Lambda__Function = ResourceType' "AWS::Lambda::Function"

pattern ResourceType_AWS__Lambda__LayerVersion :: ResourceType
pattern ResourceType_AWS__Lambda__LayerVersion = ResourceType' "AWS::Lambda::LayerVersion"

pattern ResourceType_AWS__S3__Bucket :: ResourceType
pattern ResourceType_AWS__S3__Bucket = ResourceType' "AWS::S3::Bucket"

pattern ResourceType_AWS__SQS__Queue :: ResourceType
pattern ResourceType_AWS__SQS__Queue = ResourceType' "AWS::SQS::Queue"

pattern ResourceType_AWS__SecretsManager__Secret :: ResourceType
pattern ResourceType_AWS__SecretsManager__Secret = ResourceType' "AWS::SecretsManager::Secret"

{-# COMPLETE
  ResourceType_AWS__IAM__Role,
  ResourceType_AWS__KMS__Key,
  ResourceType_AWS__Lambda__Function,
  ResourceType_AWS__Lambda__LayerVersion,
  ResourceType_AWS__S3__Bucket,
  ResourceType_AWS__SQS__Queue,
  ResourceType_AWS__SecretsManager__Secret,
  ResourceType'
  #-}
