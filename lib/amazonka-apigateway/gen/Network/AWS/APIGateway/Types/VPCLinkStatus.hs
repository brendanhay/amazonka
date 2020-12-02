{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.VPCLinkStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.VPCLinkStatus where

import Network.AWS.Prelude

data VPCLinkStatus
  = VLSAvailable
  | VLSDeleting
  | VLSFailed
  | VLSPending
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

instance FromText VPCLinkStatus where
  parser =
    takeLowerText >>= \case
      "available" -> pure VLSAvailable
      "deleting" -> pure VLSDeleting
      "failed" -> pure VLSFailed
      "pending" -> pure VLSPending
      e ->
        fromTextError $
          "Failure parsing VPCLinkStatus from value: '" <> e
            <> "'. Accepted values: available, deleting, failed, pending"

instance ToText VPCLinkStatus where
  toText = \case
    VLSAvailable -> "AVAILABLE"
    VLSDeleting -> "DELETING"
    VLSFailed -> "FAILED"
    VLSPending -> "PENDING"

instance Hashable VPCLinkStatus

instance NFData VPCLinkStatus

instance ToByteString VPCLinkStatus

instance ToQuery VPCLinkStatus

instance ToHeader VPCLinkStatus

instance FromJSON VPCLinkStatus where
  parseJSON = parseJSONText "VPCLinkStatus"
