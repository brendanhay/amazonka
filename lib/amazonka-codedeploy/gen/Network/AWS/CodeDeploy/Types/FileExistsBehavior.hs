{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.FileExistsBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.FileExistsBehavior where

import Network.AWS.Prelude

data FileExistsBehavior
  = Disallow
  | Overwrite
  | Retain
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

instance FromText FileExistsBehavior where
  parser =
    takeLowerText >>= \case
      "disallow" -> pure Disallow
      "overwrite" -> pure Overwrite
      "retain" -> pure Retain
      e ->
        fromTextError $
          "Failure parsing FileExistsBehavior from value: '" <> e
            <> "'. Accepted values: disallow, overwrite, retain"

instance ToText FileExistsBehavior where
  toText = \case
    Disallow -> "DISALLOW"
    Overwrite -> "OVERWRITE"
    Retain -> "RETAIN"

instance Hashable FileExistsBehavior

instance NFData FileExistsBehavior

instance ToByteString FileExistsBehavior

instance ToQuery FileExistsBehavior

instance ToHeader FileExistsBehavior

instance ToJSON FileExistsBehavior where
  toJSON = toJSONText

instance FromJSON FileExistsBehavior where
  parseJSON = parseJSONText "FileExistsBehavior"
