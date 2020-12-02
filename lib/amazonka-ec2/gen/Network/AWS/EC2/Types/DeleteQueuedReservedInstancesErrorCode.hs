{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteQueuedReservedInstancesErrorCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DeleteQueuedReservedInstancesErrorCode
  = DQRIECReservedInstancesIdInvalid
  | DQRIECReservedInstancesNotInQueuedState
  | DQRIECUnexpectedError
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

instance FromText DeleteQueuedReservedInstancesErrorCode where
  parser =
    takeLowerText >>= \case
      "reserved-instances-id-invalid" -> pure DQRIECReservedInstancesIdInvalid
      "reserved-instances-not-in-queued-state" -> pure DQRIECReservedInstancesNotInQueuedState
      "unexpected-error" -> pure DQRIECUnexpectedError
      e ->
        fromTextError $
          "Failure parsing DeleteQueuedReservedInstancesErrorCode from value: '" <> e
            <> "'. Accepted values: reserved-instances-id-invalid, reserved-instances-not-in-queued-state, unexpected-error"

instance ToText DeleteQueuedReservedInstancesErrorCode where
  toText = \case
    DQRIECReservedInstancesIdInvalid -> "reserved-instances-id-invalid"
    DQRIECReservedInstancesNotInQueuedState -> "reserved-instances-not-in-queued-state"
    DQRIECUnexpectedError -> "unexpected-error"

instance Hashable DeleteQueuedReservedInstancesErrorCode

instance NFData DeleteQueuedReservedInstancesErrorCode

instance ToByteString DeleteQueuedReservedInstancesErrorCode

instance ToQuery DeleteQueuedReservedInstancesErrorCode

instance ToHeader DeleteQueuedReservedInstancesErrorCode

instance FromXML DeleteQueuedReservedInstancesErrorCode where
  parseXML = parseXMLText "DeleteQueuedReservedInstancesErrorCode"
