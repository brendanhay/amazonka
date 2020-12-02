{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetErrorCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DeleteFleetErrorCode
  = DFECFleetIdDoesNotExist
  | DFECFleetIdMalformed
  | DFECFleetNotInDeletableState
  | DFECUnexpectedError
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

instance FromText DeleteFleetErrorCode where
  parser =
    takeLowerText >>= \case
      "fleetiddoesnotexist" -> pure DFECFleetIdDoesNotExist
      "fleetidmalformed" -> pure DFECFleetIdMalformed
      "fleetnotindeletablestate" -> pure DFECFleetNotInDeletableState
      "unexpectederror" -> pure DFECUnexpectedError
      e ->
        fromTextError $
          "Failure parsing DeleteFleetErrorCode from value: '" <> e
            <> "'. Accepted values: fleetiddoesnotexist, fleetidmalformed, fleetnotindeletablestate, unexpectederror"

instance ToText DeleteFleetErrorCode where
  toText = \case
    DFECFleetIdDoesNotExist -> "fleetIdDoesNotExist"
    DFECFleetIdMalformed -> "fleetIdMalformed"
    DFECFleetNotInDeletableState -> "fleetNotInDeletableState"
    DFECUnexpectedError -> "unexpectedError"

instance Hashable DeleteFleetErrorCode

instance NFData DeleteFleetErrorCode

instance ToByteString DeleteFleetErrorCode

instance ToQuery DeleteFleetErrorCode

instance ToHeader DeleteFleetErrorCode

instance FromXML DeleteFleetErrorCode where
  parseXML = parseXMLText "DeleteFleetErrorCode"
