{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SharedResourceSortByType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SharedResourceSortByType where

import Network.AWS.Prelude

data SharedResourceSortByType
  = ARN
  | ModifiedTime
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

instance FromText SharedResourceSortByType where
  parser =
    takeLowerText >>= \case
      "arn" -> pure ARN
      "modified_time" -> pure ModifiedTime
      e ->
        fromTextError $
          "Failure parsing SharedResourceSortByType from value: '" <> e
            <> "'. Accepted values: arn, modified_time"

instance ToText SharedResourceSortByType where
  toText = \case
    ARN -> "ARN"
    ModifiedTime -> "MODIFIED_TIME"

instance Hashable SharedResourceSortByType

instance NFData SharedResourceSortByType

instance ToByteString SharedResourceSortByType

instance ToQuery SharedResourceSortByType

instance ToHeader SharedResourceSortByType

instance ToJSON SharedResourceSortByType where
  toJSON = toJSONText
