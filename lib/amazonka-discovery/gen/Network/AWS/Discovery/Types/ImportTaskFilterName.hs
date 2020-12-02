{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTaskFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTaskFilterName where

import Network.AWS.Prelude

data ImportTaskFilterName
  = ImportTaskId
  | Name
  | Status
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

instance FromText ImportTaskFilterName where
  parser =
    takeLowerText >>= \case
      "import_task_id" -> pure ImportTaskId
      "name" -> pure Name
      "status" -> pure Status
      e ->
        fromTextError $
          "Failure parsing ImportTaskFilterName from value: '" <> e
            <> "'. Accepted values: import_task_id, name, status"

instance ToText ImportTaskFilterName where
  toText = \case
    ImportTaskId -> "IMPORT_TASK_ID"
    Name -> "NAME"
    Status -> "STATUS"

instance Hashable ImportTaskFilterName

instance NFData ImportTaskFilterName

instance ToByteString ImportTaskFilterName

instance ToQuery ImportTaskFilterName

instance ToHeader ImportTaskFilterName

instance ToJSON ImportTaskFilterName where
  toJSON = toJSONText
