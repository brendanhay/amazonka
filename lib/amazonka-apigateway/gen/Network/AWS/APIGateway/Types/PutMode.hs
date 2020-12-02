{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.PutMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.PutMode where

import Network.AWS.Prelude

data PutMode
  = Merge
  | Overwrite
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

instance FromText PutMode where
  parser =
    takeLowerText >>= \case
      "merge" -> pure Merge
      "overwrite" -> pure Overwrite
      e ->
        fromTextError $
          "Failure parsing PutMode from value: '" <> e
            <> "'. Accepted values: merge, overwrite"

instance ToText PutMode where
  toText = \case
    Merge -> "merge"
    Overwrite -> "overwrite"

instance Hashable PutMode

instance NFData PutMode

instance ToByteString PutMode

instance ToQuery PutMode

instance ToHeader PutMode

instance ToJSON PutMode where
  toJSON = toJSONText
