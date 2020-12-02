{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SortExperimentsBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SortExperimentsBy where

import Network.AWS.Prelude

data SortExperimentsBy
  = SEBCreationTime
  | SEBName
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

instance FromText SortExperimentsBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure SEBCreationTime
      "name" -> pure SEBName
      e ->
        fromTextError $
          "Failure parsing SortExperimentsBy from value: '" <> e
            <> "'. Accepted values: creationtime, name"

instance ToText SortExperimentsBy where
  toText = \case
    SEBCreationTime -> "CreationTime"
    SEBName -> "Name"

instance Hashable SortExperimentsBy

instance NFData SortExperimentsBy

instance ToByteString SortExperimentsBy

instance ToQuery SortExperimentsBy

instance ToHeader SortExperimentsBy

instance ToJSON SortExperimentsBy where
  toJSON = toJSONText
