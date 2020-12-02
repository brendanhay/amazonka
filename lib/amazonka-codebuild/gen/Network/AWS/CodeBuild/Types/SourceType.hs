{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceType where

import Network.AWS.Prelude

data SourceType
  = STBitbucket
  | STCodecommit
  | STCodepipeline
  | STGithub
  | STGithubEnterprise
  | STNoSource
  | STS3
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText SourceType where
  parser =
    takeLowerText >>= \case
      "bitbucket" -> pure STBitbucket
      "codecommit" -> pure STCodecommit
      "codepipeline" -> pure STCodepipeline
      "github" -> pure STGithub
      "github_enterprise" -> pure STGithubEnterprise
      "no_source" -> pure STNoSource
      "s3" -> pure STS3
      e ->
        fromTextError $
          "Failure parsing SourceType from value: '" <> e
            <> "'. Accepted values: bitbucket, codecommit, codepipeline, github, github_enterprise, no_source, s3"

instance ToText SourceType where
  toText = \case
    STBitbucket -> "BITBUCKET"
    STCodecommit -> "CODECOMMIT"
    STCodepipeline -> "CODEPIPELINE"
    STGithub -> "GITHUB"
    STGithubEnterprise -> "GITHUB_ENTERPRISE"
    STNoSource -> "NO_SOURCE"
    STS3 -> "S3"

instance Hashable SourceType

instance NFData SourceType

instance ToByteString SourceType

instance ToQuery SourceType

instance ToHeader SourceType

instance ToJSON SourceType where
  toJSON = toJSONText

instance FromJSON SourceType where
  parseJSON = parseJSONText "SourceType"
