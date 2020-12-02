{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.DeliveryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.DeliveryStatus where

import Network.AWS.Prelude

data DeliveryStatus
  = DSFailure
  | DSNotApplicable
  | DSSuccess
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

instance FromText DeliveryStatus where
  parser =
    takeLowerText >>= \case
      "failure" -> pure DSFailure
      "not_applicable" -> pure DSNotApplicable
      "success" -> pure DSSuccess
      e ->
        fromTextError $
          "Failure parsing DeliveryStatus from value: '" <> e
            <> "'. Accepted values: failure, not_applicable, success"

instance ToText DeliveryStatus where
  toText = \case
    DSFailure -> "Failure"
    DSNotApplicable -> "Not_Applicable"
    DSSuccess -> "Success"

instance Hashable DeliveryStatus

instance NFData DeliveryStatus

instance ToByteString DeliveryStatus

instance ToQuery DeliveryStatus

instance ToHeader DeliveryStatus

instance FromJSON DeliveryStatus where
  parseJSON = parseJSONText "DeliveryStatus"
