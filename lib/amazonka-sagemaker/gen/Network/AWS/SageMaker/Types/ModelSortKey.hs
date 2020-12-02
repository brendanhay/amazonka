{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelSortKey where

import Network.AWS.Prelude

data ModelSortKey
  = MSKCreationTime
  | MSKName
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

instance FromText ModelSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure MSKCreationTime
      "name" -> pure MSKName
      e ->
        fromTextError $
          "Failure parsing ModelSortKey from value: '" <> e
            <> "'. Accepted values: creationtime, name"

instance ToText ModelSortKey where
  toText = \case
    MSKCreationTime -> "CreationTime"
    MSKName -> "Name"

instance Hashable ModelSortKey

instance NFData ModelSortKey

instance ToByteString ModelSortKey

instance ToQuery ModelSortKey

instance ToHeader ModelSortKey

instance ToJSON ModelSortKey where
  toJSON = toJSONText
