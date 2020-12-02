{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceTransferType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceTransferType where

import Network.AWS.Prelude

-- | The type of device transfer. INCOMING for an input device that is being transferred to you, OUTGOING for an input device that you are transferring to another AWS account.
data InputDeviceTransferType
  = Incoming
  | Outgoing
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

instance FromText InputDeviceTransferType where
  parser =
    takeLowerText >>= \case
      "incoming" -> pure Incoming
      "outgoing" -> pure Outgoing
      e ->
        fromTextError $
          "Failure parsing InputDeviceTransferType from value: '" <> e
            <> "'. Accepted values: incoming, outgoing"

instance ToText InputDeviceTransferType where
  toText = \case
    Incoming -> "INCOMING"
    Outgoing -> "OUTGOING"

instance Hashable InputDeviceTransferType

instance NFData InputDeviceTransferType

instance ToByteString InputDeviceTransferType

instance ToQuery InputDeviceTransferType

instance ToHeader InputDeviceTransferType

instance FromJSON InputDeviceTransferType where
  parseJSON = parseJSONText "InputDeviceTransferType"
