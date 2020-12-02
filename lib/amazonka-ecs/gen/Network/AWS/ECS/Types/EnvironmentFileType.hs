{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.EnvironmentFileType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.EnvironmentFileType where

import Network.AWS.Prelude

data EnvironmentFileType = S3
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

instance FromText EnvironmentFileType where
  parser =
    takeLowerText >>= \case
      "s3" -> pure S3
      e ->
        fromTextError $
          "Failure parsing EnvironmentFileType from value: '" <> e
            <> "'. Accepted values: s3"

instance ToText EnvironmentFileType where
  toText = \case
    S3 -> "s3"

instance Hashable EnvironmentFileType

instance NFData EnvironmentFileType

instance ToByteString EnvironmentFileType

instance ToQuery EnvironmentFileType

instance ToHeader EnvironmentFileType

instance ToJSON EnvironmentFileType where
  toJSON = toJSONText

instance FromJSON EnvironmentFileType where
  parseJSON = parseJSONText "EnvironmentFileType"
