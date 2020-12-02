{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ArrayJobDependency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ArrayJobDependency where

import Network.AWS.Prelude

data ArrayJobDependency
  = NToN
  | Sequential
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

instance FromText ArrayJobDependency where
  parser =
    takeLowerText >>= \case
      "n_to_n" -> pure NToN
      "sequential" -> pure Sequential
      e ->
        fromTextError $
          "Failure parsing ArrayJobDependency from value: '" <> e
            <> "'. Accepted values: n_to_n, sequential"

instance ToText ArrayJobDependency where
  toText = \case
    NToN -> "N_TO_N"
    Sequential -> "SEQUENTIAL"

instance Hashable ArrayJobDependency

instance NFData ArrayJobDependency

instance ToByteString ArrayJobDependency

instance ToQuery ArrayJobDependency

instance ToHeader ArrayJobDependency

instance ToJSON ArrayJobDependency where
  toJSON = toJSONText

instance FromJSON ArrayJobDependency where
  parseJSON = parseJSONText "ArrayJobDependency"
