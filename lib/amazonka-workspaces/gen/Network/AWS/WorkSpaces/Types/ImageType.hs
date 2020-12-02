{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ImageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ImageType where

import Network.AWS.Prelude

data ImageType
  = ITOwned
  | ITShared
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

instance FromText ImageType where
  parser =
    takeLowerText >>= \case
      "owned" -> pure ITOwned
      "shared" -> pure ITShared
      e ->
        fromTextError $
          "Failure parsing ImageType from value: '" <> e
            <> "'. Accepted values: owned, shared"

instance ToText ImageType where
  toText = \case
    ITOwned -> "OWNED"
    ITShared -> "SHARED"

instance Hashable ImageType

instance NFData ImageType

instance ToByteString ImageType

instance ToQuery ImageType

instance ToHeader ImageType

instance ToJSON ImageType where
  toJSON = toJSONText
