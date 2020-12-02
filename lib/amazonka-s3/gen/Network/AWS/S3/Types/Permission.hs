{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Permission where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data Permission
  = PFullControl
  | PRead
  | PReadAcp
  | PWrite
  | PWriteAcp
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
      "full_control" -> pure PFullControl
      "read" -> pure PRead
      "read_acp" -> pure PReadAcp
      "write" -> pure PWrite
      "write_acp" -> pure PWriteAcp
      e ->
        fromTextError $
          "Failure parsing Permission from value: '" <> e
            <> "'. Accepted values: full_control, read, read_acp, write, write_acp"

instance ToText Permission where
  toText = \case
    PFullControl -> "FULL_CONTROL"
    PRead -> "READ"
    PReadAcp -> "READ_ACP"
    PWrite -> "WRITE"
    PWriteAcp -> "WRITE_ACP"

instance Hashable Permission

instance NFData Permission

instance ToByteString Permission

instance ToQuery Permission

instance ToHeader Permission

instance FromXML Permission where
  parseXML = parseXMLText "Permission"

instance ToXML Permission where
  toXML = toXMLText
