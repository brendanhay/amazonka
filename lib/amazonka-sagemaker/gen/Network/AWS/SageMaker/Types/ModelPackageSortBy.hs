{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelPackageSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelPackageSortBy where

import Network.AWS.Prelude

data ModelPackageSortBy
  = MPSBCreationTime
  | MPSBName
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

instance FromText ModelPackageSortBy where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure MPSBCreationTime
      "name" -> pure MPSBName
      e ->
        fromTextError $
          "Failure parsing ModelPackageSortBy from value: '" <> e
            <> "'. Accepted values: creationtime, name"

instance ToText ModelPackageSortBy where
  toText = \case
    MPSBCreationTime -> "CreationTime"
    MPSBName -> "Name"

instance Hashable ModelPackageSortBy

instance NFData ModelPackageSortBy

instance ToByteString ModelPackageSortBy

instance ToQuery ModelPackageSortBy

instance ToHeader ModelPackageSortBy

instance ToJSON ModelPackageSortBy where
  toJSON = toJSONText
