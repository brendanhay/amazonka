{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.QueueListBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.QueueListBy where

import Network.AWS.Prelude

-- | Optional. When you request a list of queues, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by creation date.
data QueueListBy
  = CreationDate
  | Name
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

instance FromText QueueListBy where
  parser =
    takeLowerText >>= \case
      "creation_date" -> pure CreationDate
      "name" -> pure Name
      e ->
        fromTextError $
          "Failure parsing QueueListBy from value: '" <> e
            <> "'. Accepted values: creation_date, name"

instance ToText QueueListBy where
  toText = \case
    CreationDate -> "CREATION_DATE"
    Name -> "NAME"

instance Hashable QueueListBy

instance NFData QueueListBy

instance ToByteString QueueListBy

instance ToQuery QueueListBy

instance ToHeader QueueListBy

instance ToJSON QueueListBy where
  toJSON = toJSONText
