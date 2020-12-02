{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentStatusType where

import Network.AWS.Prelude

data DocumentStatusType
  = DSTActive
  | DSTInitialized
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

instance FromText DocumentStatusType where
  parser =
    takeLowerText >>= \case
      "active" -> pure DSTActive
      "initialized" -> pure DSTInitialized
      e ->
        fromTextError $
          "Failure parsing DocumentStatusType from value: '" <> e
            <> "'. Accepted values: active, initialized"

instance ToText DocumentStatusType where
  toText = \case
    DSTActive -> "ACTIVE"
    DSTInitialized -> "INITIALIZED"

instance Hashable DocumentStatusType

instance NFData DocumentStatusType

instance ToByteString DocumentStatusType

instance ToQuery DocumentStatusType

instance ToHeader DocumentStatusType

instance FromJSON DocumentStatusType where
  parseJSON = parseJSONText "DocumentStatusType"
