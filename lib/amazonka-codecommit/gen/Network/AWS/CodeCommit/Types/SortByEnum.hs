{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.SortByEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.SortByEnum where

import Network.AWS.Prelude

data SortByEnum
  = LastModifiedDate
  | RepositoryName
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

instance FromText SortByEnum where
  parser =
    takeLowerText >>= \case
      "lastmodifieddate" -> pure LastModifiedDate
      "repositoryname" -> pure RepositoryName
      e ->
        fromTextError $
          "Failure parsing SortByEnum from value: '" <> e
            <> "'. Accepted values: lastmodifieddate, repositoryname"

instance ToText SortByEnum where
  toText = \case
    LastModifiedDate -> "lastModifiedDate"
    RepositoryName -> "repositoryName"

instance Hashable SortByEnum

instance NFData SortByEnum

instance ToByteString SortByEnum

instance ToQuery SortByEnum

instance ToHeader SortByEnum

instance ToJSON SortByEnum where
  toJSON = toJSONText
