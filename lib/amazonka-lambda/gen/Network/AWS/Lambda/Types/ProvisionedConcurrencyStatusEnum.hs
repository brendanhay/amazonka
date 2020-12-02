{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum where

import Network.AWS.Prelude

data ProvisionedConcurrencyStatusEnum
  = PCSEFailed
  | PCSEInProgress
  | PCSEReady
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

instance FromText ProvisionedConcurrencyStatusEnum where
  parser =
    takeLowerText >>= \case
      "failed" -> pure PCSEFailed
      "in_progress" -> pure PCSEInProgress
      "ready" -> pure PCSEReady
      e ->
        fromTextError $
          "Failure parsing ProvisionedConcurrencyStatusEnum from value: '" <> e
            <> "'. Accepted values: failed, in_progress, ready"

instance ToText ProvisionedConcurrencyStatusEnum where
  toText = \case
    PCSEFailed -> "FAILED"
    PCSEInProgress -> "IN_PROGRESS"
    PCSEReady -> "READY"

instance Hashable ProvisionedConcurrencyStatusEnum

instance NFData ProvisionedConcurrencyStatusEnum

instance ToByteString ProvisionedConcurrencyStatusEnum

instance ToQuery ProvisionedConcurrencyStatusEnum

instance ToHeader ProvisionedConcurrencyStatusEnum

instance FromJSON ProvisionedConcurrencyStatusEnum where
  parseJSON = parseJSONText "ProvisionedConcurrencyStatusEnum"
