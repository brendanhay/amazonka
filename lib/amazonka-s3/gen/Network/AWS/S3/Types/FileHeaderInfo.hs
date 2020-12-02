{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.FileHeaderInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.FileHeaderInfo where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data FileHeaderInfo
  = Ignore
  | None
  | Use
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

instance FromText FileHeaderInfo where
  parser =
    takeLowerText >>= \case
      "ignore" -> pure Ignore
      "none" -> pure None
      "use" -> pure Use
      e ->
        fromTextError $
          "Failure parsing FileHeaderInfo from value: '" <> e
            <> "'. Accepted values: ignore, none, use"

instance ToText FileHeaderInfo where
  toText = \case
    Ignore -> "IGNORE"
    None -> "NONE"
    Use -> "USE"

instance Hashable FileHeaderInfo

instance NFData FileHeaderInfo

instance ToByteString FileHeaderInfo

instance ToQuery FileHeaderInfo

instance ToHeader FileHeaderInfo

instance ToXML FileHeaderInfo where
  toXML = toXMLText
