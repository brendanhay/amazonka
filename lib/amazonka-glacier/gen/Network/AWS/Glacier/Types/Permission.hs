{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Permission where

import Network.AWS.Prelude

data Permission
  = FullControl
  | Read
  | ReadAcp
  | Write
  | WriteAcp
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

instance FromText Permission where
  parser =
    takeLowerText >>= \case
      "full_control" -> pure FullControl
      "read" -> pure Read
      "read_acp" -> pure ReadAcp
      "write" -> pure Write
      "write_acp" -> pure WriteAcp
      e ->
        fromTextError $
          "Failure parsing Permission from value: '" <> e
            <> "'. Accepted values: full_control, read, read_acp, write, write_acp"

instance ToText Permission where
  toText = \case
    FullControl -> "FULL_CONTROL"
    Read -> "READ"
    ReadAcp -> "READ_ACP"
    Write -> "WRITE"
    WriteAcp -> "WRITE_ACP"

instance Hashable Permission

instance NFData Permission

instance ToByteString Permission

instance ToQuery Permission

instance ToHeader Permission

instance ToJSON Permission where
  toJSON = toJSONText

instance FromJSON Permission where
  parseJSON = parseJSONText "Permission"
