{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Amazonka.DynamoDB.Types.TransactWriteItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TransactWriteItem where

import {-# SOURCE #-} Amazonka.DynamoDB.Types.ConditionCheck
import {-# SOURCE #-} Amazonka.DynamoDB.Types.Delete
import {-# SOURCE #-} Amazonka.DynamoDB.Types.Put
import {-# SOURCE #-} Amazonka.DynamoDB.Types.Update
import Amazonka.Prelude
import Data.Aeson (ToJSON (..), object, pairs, (.=))

-- | A transactional request that can perform update, put, delete, or
-- check operations on multiple items in one or more tables
-- atomically.
--
-- For more information, see
--
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_TransactWriteItems.html TransactWriteItems>
-- in the /Amazon DynamoDB Developer Guide/.
data TransactWriteItem
  = -- | A request to perform a check item operation.
    ConditionCheck ConditionCheck
  | -- | A request to perform a @DeleteItem@ operation.
    Delete Delete
  | -- | A request to perform a @PutItem@ operation.
    Put Put
  | -- | A request to perform an @UpdateItem@ operation.
    Update Update
  deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON TransactWriteItem where
  toJSON =
    object . pure . \case
      ConditionCheck cc -> "ConditionCheck" .= cc
      Delete d -> "Delete" .= d
      Put p -> "Put" .= p
      Update u -> "Update" .= u

  toEncoding =
    pairs . \case
      ConditionCheck cc -> "ConditionCheck" .= cc
      Delete d -> "Delete" .= d
      Put p -> "Put" .= p
      Update u -> "Update" .= u
