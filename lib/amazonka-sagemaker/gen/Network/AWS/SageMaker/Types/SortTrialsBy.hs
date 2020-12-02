{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortTrialsBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SortTrialsBy where

import Network.AWS.Prelude

data SortTrialsBy
  = STBCreationTime
  | STBName
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

instance FromText SortTrialsBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure STBCreationTime
      "name" -> pure STBName
      e ->
        fromTextError $
          "Failure parsing SortTrialsBy from value: '" <> e
            <> "'. Accepted values: creationtime, name"

instance ToText SortTrialsBy where
  toText = \case
    STBCreationTime -> "CreationTime"
    STBName -> "Name"

instance Hashable SortTrialsBy

instance NFData SortTrialsBy

instance ToByteString SortTrialsBy

instance ToQuery SortTrialsBy

instance ToHeader SortTrialsBy

instance ToJSON SortTrialsBy where
  toJSON = toJSONText
