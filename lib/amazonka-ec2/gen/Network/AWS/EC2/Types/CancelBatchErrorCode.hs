{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelBatchErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelBatchErrorCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CancelBatchErrorCode
  = CBECFleetRequestIdDoesNotExist
  | CBECFleetRequestIdMalformed
  | CBECFleetRequestNotInCancellableState
  | CBECUnexpectedError
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

instance FromText CancelBatchErrorCode where
  parser =
    takeLowerText >>= \case
      "fleetrequestiddoesnotexist" -> pure CBECFleetRequestIdDoesNotExist
      "fleetrequestidmalformed" -> pure CBECFleetRequestIdMalformed
      "fleetrequestnotincancellablestate" -> pure CBECFleetRequestNotInCancellableState
      "unexpectederror" -> pure CBECUnexpectedError
      e ->
        fromTextError $
          "Failure parsing CancelBatchErrorCode from value: '" <> e
            <> "'. Accepted values: fleetrequestiddoesnotexist, fleetrequestidmalformed, fleetrequestnotincancellablestate, unexpectederror"

instance ToText CancelBatchErrorCode where
  toText = \case
    CBECFleetRequestIdDoesNotExist -> "fleetRequestIdDoesNotExist"
    CBECFleetRequestIdMalformed -> "fleetRequestIdMalformed"
    CBECFleetRequestNotInCancellableState -> "fleetRequestNotInCancellableState"
    CBECUnexpectedError -> "unexpectedError"

instance Hashable CancelBatchErrorCode

instance NFData CancelBatchErrorCode

instance ToByteString CancelBatchErrorCode

instance ToQuery CancelBatchErrorCode

instance ToHeader CancelBatchErrorCode

instance FromXML CancelBatchErrorCode where
  parseXML = parseXMLText "CancelBatchErrorCode"
