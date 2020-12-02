{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CodeRepositorySortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CodeRepositorySortBy where

import Network.AWS.Prelude

data CodeRepositorySortBy
  = CRSBCreationTime
  | CRSBLastModifiedTime
  | CRSBName
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

instance FromText CodeRepositorySortBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure CRSBCreationTime
      "lastmodifiedtime" -> pure CRSBLastModifiedTime
      "name" -> pure CRSBName
      e ->
        fromTextError $
          "Failure parsing CodeRepositorySortBy from value: '" <> e
            <> "'. Accepted values: creationtime, lastmodifiedtime, name"

instance ToText CodeRepositorySortBy where
  toText = \case
    CRSBCreationTime -> "CreationTime"
    CRSBLastModifiedTime -> "LastModifiedTime"
    CRSBName -> "Name"

instance Hashable CodeRepositorySortBy

instance NFData CodeRepositorySortBy

instance ToByteString CodeRepositorySortBy

instance ToQuery CodeRepositorySortBy

instance ToHeader CodeRepositorySortBy

instance ToJSON CodeRepositorySortBy where
  toJSON = toJSONText
