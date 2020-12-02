{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.SourceType where

import Network.AWS.Prelude

data SourceType
  = Archive
  | Git
  | S3
  | SVN
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
      "archive" -> pure Archive
      "git" -> pure Git
      "s3" -> pure S3
      "svn" -> pure SVN
      e ->
        fromTextError $
          "Failure parsing SourceType from value: '" <> e
            <> "'. Accepted values: archive, git, s3, svn"

instance ToText SourceType where
  toText = \case
    Archive -> "archive"
    Git -> "git"
    S3 -> "s3"
    SVN -> "svn"

instance Hashable SourceType

instance NFData SourceType

instance ToByteString SourceType

instance ToQuery SourceType

instance ToHeader SourceType

instance ToJSON SourceType where
  toJSON = toJSONText

instance FromJSON SourceType where
  parseJSON = parseJSONText "SourceType"
