{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobSortByOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobSortByOptions where

import Network.AWS.Prelude

data HyperParameterTuningJobSortByOptions
  = HPTJSBOCreationTime
  | HPTJSBOName
  | HPTJSBOStatus
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

instance FromText HyperParameterTuningJobSortByOptions where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure HPTJSBOCreationTime
      "name" -> pure HPTJSBOName
      "status" -> pure HPTJSBOStatus
      e ->
        fromTextError $
          "Failure parsing HyperParameterTuningJobSortByOptions from value: '" <> e
            <> "'. Accepted values: creationtime, name, status"

instance ToText HyperParameterTuningJobSortByOptions where
  toText = \case
    HPTJSBOCreationTime -> "CreationTime"
    HPTJSBOName -> "Name"
    HPTJSBOStatus -> "Status"

instance Hashable HyperParameterTuningJobSortByOptions

instance NFData HyperParameterTuningJobSortByOptions

instance ToByteString HyperParameterTuningJobSortByOptions

instance ToQuery HyperParameterTuningJobSortByOptions

instance ToHeader HyperParameterTuningJobSortByOptions

instance ToJSON HyperParameterTuningJobSortByOptions where
  toJSON = toJSONText
