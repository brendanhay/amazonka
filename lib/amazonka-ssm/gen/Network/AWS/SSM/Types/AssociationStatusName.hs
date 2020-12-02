{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationStatusName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationStatusName where

import Network.AWS.Prelude

data AssociationStatusName
  = ASNFailed
  | ASNPending
  | ASNSuccess
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

instance FromText AssociationStatusName where
  parser =
    takeLowerText >>= \case
      "failed" -> pure ASNFailed
      "pending" -> pure ASNPending
      "success" -> pure ASNSuccess
      e ->
        fromTextError $
          "Failure parsing AssociationStatusName from value: '" <> e
            <> "'. Accepted values: failed, pending, success"

instance ToText AssociationStatusName where
  toText = \case
    ASNFailed -> "Failed"
    ASNPending -> "Pending"
    ASNSuccess -> "Success"

instance Hashable AssociationStatusName

instance NFData AssociationStatusName

instance ToByteString AssociationStatusName

instance ToQuery AssociationStatusName

instance ToHeader AssociationStatusName

instance ToJSON AssociationStatusName where
  toJSON = toJSONText

instance FromJSON AssociationStatusName where
  parseJSON = parseJSONText "AssociationStatusName"
