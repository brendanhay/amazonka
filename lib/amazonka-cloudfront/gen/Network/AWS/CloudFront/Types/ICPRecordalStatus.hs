{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.ICPRecordalStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.ICPRecordalStatus where

import Network.AWS.Prelude

data ICPRecordalStatus
  = Approved
  | Pending
  | Suspended
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

instance FromText ICPRecordalStatus where
  parser =
    takeLowerText >>= \case
      "approved" -> pure Approved
      "pending" -> pure Pending
      "suspended" -> pure Suspended
      e ->
        fromTextError $
          "Failure parsing ICPRecordalStatus from value: '" <> e
            <> "'. Accepted values: approved, pending, suspended"

instance ToText ICPRecordalStatus where
  toText = \case
    Approved -> "APPROVED"
    Pending -> "PENDING"
    Suspended -> "SUSPENDED"

instance Hashable ICPRecordalStatus

instance NFData ICPRecordalStatus

instance ToByteString ICPRecordalStatus

instance ToQuery ICPRecordalStatus

instance ToHeader ICPRecordalStatus

instance FromXML ICPRecordalStatus where
  parseXML = parseXMLText "ICPRecordalStatus"
