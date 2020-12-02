{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ImagePullCredentialsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ImagePullCredentialsType where

import Network.AWS.Prelude

data ImagePullCredentialsType
  = Codebuild
  | ServiceRole
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

instance FromText ImagePullCredentialsType where
  parser =
    takeLowerText >>= \case
      "codebuild" -> pure Codebuild
      "service_role" -> pure ServiceRole
      e ->
        fromTextError $
          "Failure parsing ImagePullCredentialsType from value: '" <> e
            <> "'. Accepted values: codebuild, service_role"

instance ToText ImagePullCredentialsType where
  toText = \case
    Codebuild -> "CODEBUILD"
    ServiceRole -> "SERVICE_ROLE"

instance Hashable ImagePullCredentialsType

instance NFData ImagePullCredentialsType

instance ToByteString ImagePullCredentialsType

instance ToQuery ImagePullCredentialsType

instance ToHeader ImagePullCredentialsType

instance ToJSON ImagePullCredentialsType where
  toJSON = toJSONText

instance FromJSON ImagePullCredentialsType where
  parseJSON = parseJSONText "ImagePullCredentialsType"
