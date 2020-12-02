{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.JobType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.JobType where

import Network.AWS.Prelude

data JobType
  = Export
  | Import
  | LocalUse
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

instance FromText JobType where
  parser =
    takeLowerText >>= \case
      "export" -> pure Export
      "import" -> pure Import
      "local_use" -> pure LocalUse
      e ->
        fromTextError $
          "Failure parsing JobType from value: '" <> e
            <> "'. Accepted values: export, import, local_use"

instance ToText JobType where
  toText = \case
    Export -> "EXPORT"
    Import -> "IMPORT"
    LocalUse -> "LOCAL_USE"

instance Hashable JobType

instance NFData JobType

instance ToByteString JobType

instance ToQuery JobType

instance ToHeader JobType

instance ToJSON JobType where
  toJSON = toJSONText

instance FromJSON JobType where
  parseJSON = parseJSONText "JobType"
