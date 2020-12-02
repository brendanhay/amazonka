{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SortBy where

import Network.AWS.Prelude

data SortBy
  = SBCreationTime
  | SBName
  | SBStatus
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

instance FromText SortBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure SBCreationTime
      "name" -> pure SBName
      "status" -> pure SBStatus
      e ->
        fromTextError $
          "Failure parsing SortBy from value: '" <> e
            <> "'. Accepted values: creationtime, name, status"

instance ToText SortBy where
  toText = \case
    SBCreationTime -> "CreationTime"
    SBName -> "Name"
    SBStatus -> "Status"

instance Hashable SortBy

instance NFData SortBy

instance ToByteString SortBy

instance ToQuery SortBy

instance ToHeader SortBy

instance ToJSON SortBy where
  toJSON = toJSONText
