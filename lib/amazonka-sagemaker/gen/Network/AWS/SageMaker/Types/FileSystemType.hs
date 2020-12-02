{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FileSystemType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FileSystemType where

import Network.AWS.Prelude

data FileSystemType
  = Efs
  | FSxLustre
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

instance FromText FileSystemType where
  parser =
    takeLowerText >>= \case
      "efs" -> pure Efs
      "fsxlustre" -> pure FSxLustre
      e ->
        fromTextError $
          "Failure parsing FileSystemType from value: '" <> e
            <> "'. Accepted values: efs, fsxlustre"

instance ToText FileSystemType where
  toText = \case
    Efs -> "EFS"
    FSxLustre -> "FSxLustre"

instance Hashable FileSystemType

instance NFData FileSystemType

instance ToByteString FileSystemType

instance ToQuery FileSystemType

instance ToHeader FileSystemType

instance ToJSON FileSystemType where
  toJSON = toJSONText

instance FromJSON FileSystemType where
  parseJSON = parseJSONText "FileSystemType"
