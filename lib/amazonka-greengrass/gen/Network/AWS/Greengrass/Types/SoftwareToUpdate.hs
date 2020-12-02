{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SoftwareToUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.SoftwareToUpdate where

import Network.AWS.Prelude

-- | The piece of software on the Greengrass core that will be updated.
data SoftwareToUpdate
  = Core
  | OtaAgent
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

instance FromText SoftwareToUpdate where
  parser =
    takeLowerText >>= \case
      "core" -> pure Core
      "ota_agent" -> pure OtaAgent
      e ->
        fromTextError $
          "Failure parsing SoftwareToUpdate from value: '" <> e
            <> "'. Accepted values: core, ota_agent"

instance ToText SoftwareToUpdate where
  toText = \case
    Core -> "core"
    OtaAgent -> "ota_agent"

instance Hashable SoftwareToUpdate

instance NFData SoftwareToUpdate

instance ToByteString SoftwareToUpdate

instance ToQuery SoftwareToUpdate

instance ToHeader SoftwareToUpdate

instance ToJSON SoftwareToUpdate where
  toJSON = toJSONText
