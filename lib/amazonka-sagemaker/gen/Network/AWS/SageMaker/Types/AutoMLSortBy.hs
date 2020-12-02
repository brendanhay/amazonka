{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLSortBy where

import Network.AWS.Prelude

data AutoMLSortBy
  = AMLSBCreationTime
  | AMLSBName
  | AMLSBStatus
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

instance FromText AutoMLSortBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure AMLSBCreationTime
      "name" -> pure AMLSBName
      "status" -> pure AMLSBStatus
      e ->
        fromTextError $
          "Failure parsing AutoMLSortBy from value: '" <> e
            <> "'. Accepted values: creationtime, name, status"

instance ToText AutoMLSortBy where
  toText = \case
    AMLSBCreationTime -> "CreationTime"
    AMLSBName -> "Name"
    AMLSBStatus -> "Status"

instance Hashable AutoMLSortBy

instance NFData AutoMLSortBy

instance ToByteString AutoMLSortBy

instance ToQuery AutoMLSortBy

instance ToHeader AutoMLSortBy

instance ToJSON AutoMLSortBy where
  toJSON = toJSONText
