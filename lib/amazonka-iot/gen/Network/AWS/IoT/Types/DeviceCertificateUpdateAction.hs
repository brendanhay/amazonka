{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DeviceCertificateUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DeviceCertificateUpdateAction where

import Network.AWS.Prelude

data DeviceCertificateUpdateAction = DCUADeactivate
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

instance FromText DeviceCertificateUpdateAction where
  parser =
    takeLowerText >>= \case
      "deactivate" -> pure DCUADeactivate
      e ->
        fromTextError $
          "Failure parsing DeviceCertificateUpdateAction from value: '" <> e
            <> "'. Accepted values: deactivate"

instance ToText DeviceCertificateUpdateAction where
  toText = \case
    DCUADeactivate -> "DEACTIVATE"

instance Hashable DeviceCertificateUpdateAction

instance NFData DeviceCertificateUpdateAction

instance ToByteString DeviceCertificateUpdateAction

instance ToQuery DeviceCertificateUpdateAction

instance ToHeader DeviceCertificateUpdateAction

instance ToJSON DeviceCertificateUpdateAction where
  toJSON = toJSONText

instance FromJSON DeviceCertificateUpdateAction where
  parseJSON = parseJSONText "DeviceCertificateUpdateAction"
