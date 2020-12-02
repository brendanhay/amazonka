{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.DedicatedTenancyModificationStateEnum where

import Network.AWS.Prelude

data DedicatedTenancyModificationStateEnum
  = Completed
  | Failed
  | Pending
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

instance FromText DedicatedTenancyModificationStateEnum where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "failed" -> pure Failed
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing DedicatedTenancyModificationStateEnum from value: '" <> e
            <> "'. Accepted values: completed, failed, pending"

instance ToText DedicatedTenancyModificationStateEnum where
  toText = \case
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    Pending -> "PENDING"

instance Hashable DedicatedTenancyModificationStateEnum

instance NFData DedicatedTenancyModificationStateEnum

instance ToByteString DedicatedTenancyModificationStateEnum

instance ToQuery DedicatedTenancyModificationStateEnum

instance ToHeader DedicatedTenancyModificationStateEnum

instance FromJSON DedicatedTenancyModificationStateEnum where
  parseJSON = parseJSONText "DedicatedTenancyModificationStateEnum"
