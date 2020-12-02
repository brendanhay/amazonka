{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociationStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociationStatusCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data AssociationStatusCode
  = ASCAssociated
  | ASCAssociating
  | ASCAssociationFailed
  | ASCDisassociated
  | ASCDisassociating
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

instance FromText AssociationStatusCode where
  parser =
    takeLowerText >>= \case
      "associated" -> pure ASCAssociated
      "associating" -> pure ASCAssociating
      "association-failed" -> pure ASCAssociationFailed
      "disassociated" -> pure ASCDisassociated
      "disassociating" -> pure ASCDisassociating
      e ->
        fromTextError $
          "Failure parsing AssociationStatusCode from value: '" <> e
            <> "'. Accepted values: associated, associating, association-failed, disassociated, disassociating"

instance ToText AssociationStatusCode where
  toText = \case
    ASCAssociated -> "associated"
    ASCAssociating -> "associating"
    ASCAssociationFailed -> "association-failed"
    ASCDisassociated -> "disassociated"
    ASCDisassociating -> "disassociating"

instance Hashable AssociationStatusCode

instance NFData AssociationStatusCode

instance ToByteString AssociationStatusCode

instance ToQuery AssociationStatusCode

instance ToHeader AssociationStatusCode

instance FromXML AssociationStatusCode where
  parseXML = parseXMLText "AssociationStatusCode"
