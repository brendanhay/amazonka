{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmSortBy where

import Network.AWS.Prelude

data AlgorithmSortBy
  = ASBCreationTime
  | ASBName
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

instance FromText AlgorithmSortBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure ASBCreationTime
      "name" -> pure ASBName
      e ->
        fromTextError $
          "Failure parsing AlgorithmSortBy from value: '" <> e
            <> "'. Accepted values: creationtime, name"

instance ToText AlgorithmSortBy where
  toText = \case
    ASBCreationTime -> "CreationTime"
    ASBName -> "Name"

instance Hashable AlgorithmSortBy

instance NFData AlgorithmSortBy

instance ToByteString AlgorithmSortBy

instance ToQuery AlgorithmSortBy

instance ToHeader AlgorithmSortBy

instance ToJSON AlgorithmSortBy where
  toJSON = toJSONText
