{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FileSystemAccessMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FileSystemAccessMode where

import Network.AWS.Prelude

data FileSystemAccessMode
  = RO
  | RW
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

instance FromText FileSystemAccessMode where
  parser =
    takeLowerText >>= \case
      "ro" -> pure RO
      "rw" -> pure RW
      e ->
        fromTextError $
          "Failure parsing FileSystemAccessMode from value: '" <> e
            <> "'. Accepted values: ro, rw"

instance ToText FileSystemAccessMode where
  toText = \case
    RO -> "ro"
    RW -> "rw"

instance Hashable FileSystemAccessMode

instance NFData FileSystemAccessMode

instance ToByteString FileSystemAccessMode

instance ToQuery FileSystemAccessMode

instance ToHeader FileSystemAccessMode

instance ToJSON FileSystemAccessMode where
  toJSON = toJSONText

instance FromJSON FileSystemAccessMode where
  parseJSON = parseJSONText "FileSystemAccessMode"
