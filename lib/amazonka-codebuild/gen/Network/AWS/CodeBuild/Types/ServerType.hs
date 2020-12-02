{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ServerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ServerType where

import Network.AWS.Prelude

data ServerType
  = Bitbucket
  | Github
  | GithubEnterprise
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

instance FromText ServerType where
  parser =
    takeLowerText >>= \case
      "bitbucket" -> pure Bitbucket
      "github" -> pure Github
      "github_enterprise" -> pure GithubEnterprise
      e ->
        fromTextError $
          "Failure parsing ServerType from value: '" <> e
            <> "'. Accepted values: bitbucket, github, github_enterprise"

instance ToText ServerType where
  toText = \case
    Bitbucket -> "BITBUCKET"
    Github -> "GITHUB"
    GithubEnterprise -> "GITHUB_ENTERPRISE"

instance Hashable ServerType

instance NFData ServerType

instance ToByteString ServerType

instance ToQuery ServerType

instance ToHeader ServerType

instance ToJSON ServerType where
  toJSON = toJSONText

instance FromJSON ServerType where
  parseJSON = parseJSONText "ServerType"
