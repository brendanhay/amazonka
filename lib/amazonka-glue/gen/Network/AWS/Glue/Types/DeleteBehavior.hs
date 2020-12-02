{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DeleteBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DeleteBehavior where

import Network.AWS.Prelude

data DeleteBehavior
  = DeleteFromDatabase
  | DeprecateInDatabase
  | Log
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

instance FromText DeleteBehavior where
  parser =
    takeLowerText >>= \case
      "delete_from_database" -> pure DeleteFromDatabase
      "deprecate_in_database" -> pure DeprecateInDatabase
      "log" -> pure Log
      e ->
        fromTextError $
          "Failure parsing DeleteBehavior from value: '" <> e
            <> "'. Accepted values: delete_from_database, deprecate_in_database, log"

instance ToText DeleteBehavior where
  toText = \case
    DeleteFromDatabase -> "DELETE_FROM_DATABASE"
    DeprecateInDatabase -> "DEPRECATE_IN_DATABASE"
    Log -> "LOG"

instance Hashable DeleteBehavior

instance NFData DeleteBehavior

instance ToByteString DeleteBehavior

instance ToQuery DeleteBehavior

instance ToHeader DeleteBehavior

instance ToJSON DeleteBehavior where
  toJSON = toJSONText

instance FromJSON DeleteBehavior where
  parseJSON = parseJSONText "DeleteBehavior"
