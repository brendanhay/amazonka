{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.Algorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.Algorithm where

import Network.AWS.Prelude

-- | The function used to train an @MLModel@ . Training choices supported by Amazon ML include the following:
--
--
--     * @SGD@ - Stochastic Gradient Descent.    * @RandomForest@ - Random forest of decision trees.
data Algorithm = SGD
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

instance FromText Algorithm where
  parser =
    takeLowerText >>= \case
      "sgd" -> pure SGD
      e ->
        fromTextError $
          "Failure parsing Algorithm from value: '" <> e
            <> "'. Accepted values: sgd"

instance ToText Algorithm where
  toText = \case
    SGD -> "sgd"

instance Hashable Algorithm

instance NFData Algorithm

instance ToByteString Algorithm

instance ToQuery Algorithm

instance ToHeader Algorithm

instance FromJSON Algorithm where
  parseJSON = parseJSONText "Algorithm"
