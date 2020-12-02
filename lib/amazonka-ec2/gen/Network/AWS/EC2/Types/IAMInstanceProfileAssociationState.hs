{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IAMInstanceProfileAssociationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IAMInstanceProfileAssociationState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data IAMInstanceProfileAssociationState
  = IAPASAssociated
  | IAPASAssociating
  | IAPASDisassociated
  | IAPASDisassociating
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

instance FromText IAMInstanceProfileAssociationState where
  parser =
    takeLowerText >>= \case
      "associated" -> pure IAPASAssociated
      "associating" -> pure IAPASAssociating
      "disassociated" -> pure IAPASDisassociated
      "disassociating" -> pure IAPASDisassociating
      e ->
        fromTextError $
          "Failure parsing IAMInstanceProfileAssociationState from value: '" <> e
            <> "'. Accepted values: associated, associating, disassociated, disassociating"

instance ToText IAMInstanceProfileAssociationState where
  toText = \case
    IAPASAssociated -> "associated"
    IAPASAssociating -> "associating"
    IAPASDisassociated -> "disassociated"
    IAPASDisassociating -> "disassociating"

instance Hashable IAMInstanceProfileAssociationState

instance NFData IAMInstanceProfileAssociationState

instance ToByteString IAMInstanceProfileAssociationState

instance ToQuery IAMInstanceProfileAssociationState

instance ToHeader IAMInstanceProfileAssociationState

instance FromXML IAMInstanceProfileAssociationState where
  parseXML = parseXMLText "IAMInstanceProfileAssociationState"
