{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationSyncCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationSyncCompliance where

import Network.AWS.Prelude

data AssociationSyncCompliance
  = Auto
  | Manual
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

instance FromText AssociationSyncCompliance where
  parser =
    takeLowerText >>= \case
      "auto" -> pure Auto
      "manual" -> pure Manual
      e ->
        fromTextError $
          "Failure parsing AssociationSyncCompliance from value: '" <> e
            <> "'. Accepted values: auto, manual"

instance ToText AssociationSyncCompliance where
  toText = \case
    Auto -> "AUTO"
    Manual -> "MANUAL"

instance Hashable AssociationSyncCompliance

instance NFData AssociationSyncCompliance

instance ToByteString AssociationSyncCompliance

instance ToQuery AssociationSyncCompliance

instance ToHeader AssociationSyncCompliance

instance ToJSON AssociationSyncCompliance where
  toJSON = toJSONText

instance FromJSON AssociationSyncCompliance where
  parseJSON = parseJSONText "AssociationSyncCompliance"
