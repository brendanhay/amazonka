{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.RetryBuildBatchType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.RetryBuildBatchType where

import Network.AWS.Prelude

data RetryBuildBatchType
  = RetryAllBuilds
  | RetryFailedBuilds
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

instance FromText RetryBuildBatchType where
  parser =
    takeLowerText >>= \case
      "retry_all_builds" -> pure RetryAllBuilds
      "retry_failed_builds" -> pure RetryFailedBuilds
      e ->
        fromTextError $
          "Failure parsing RetryBuildBatchType from value: '" <> e
            <> "'. Accepted values: retry_all_builds, retry_failed_builds"

instance ToText RetryBuildBatchType where
  toText = \case
    RetryAllBuilds -> "RETRY_ALL_BUILDS"
    RetryFailedBuilds -> "RETRY_FAILED_BUILDS"

instance Hashable RetryBuildBatchType

instance NFData RetryBuildBatchType

instance ToByteString RetryBuildBatchType

instance ToQuery RetryBuildBatchType

instance ToHeader RetryBuildBatchType

instance ToJSON RetryBuildBatchType where
  toJSON = toJSONText
