{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.RepositoryAccessMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RepositoryAccessMode where

import Network.AWS.Prelude

data RepositoryAccessMode
  = Platform
  | VPC
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

instance FromText RepositoryAccessMode where
  parser =
    takeLowerText >>= \case
      "platform" -> pure Platform
      "vpc" -> pure VPC
      e ->
        fromTextError $
          "Failure parsing RepositoryAccessMode from value: '" <> e
            <> "'. Accepted values: platform, vpc"

instance ToText RepositoryAccessMode where
  toText = \case
    Platform -> "Platform"
    VPC -> "Vpc"

instance Hashable RepositoryAccessMode

instance NFData RepositoryAccessMode

instance ToByteString RepositoryAccessMode

instance ToQuery RepositoryAccessMode

instance ToHeader RepositoryAccessMode

instance ToJSON RepositoryAccessMode where
  toJSON = toJSONText

instance FromJSON RepositoryAccessMode where
  parseJSON = parseJSONText "RepositoryAccessMode"
