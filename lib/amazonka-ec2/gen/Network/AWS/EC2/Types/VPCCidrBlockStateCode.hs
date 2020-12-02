{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCCidrBlockStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCCidrBlockStateCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCCidrBlockStateCode
  = VCBSCAssociated
  | VCBSCAssociating
  | VCBSCDisassociated
  | VCBSCDisassociating
  | VCBSCFailed
  | VCBSCFailing
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

instance FromText VPCCidrBlockStateCode where
  parser =
    takeLowerText >>= \case
      "associated" -> pure VCBSCAssociated
      "associating" -> pure VCBSCAssociating
      "disassociated" -> pure VCBSCDisassociated
      "disassociating" -> pure VCBSCDisassociating
      "failed" -> pure VCBSCFailed
      "failing" -> pure VCBSCFailing
      e ->
        fromTextError $
          "Failure parsing VPCCidrBlockStateCode from value: '" <> e
            <> "'. Accepted values: associated, associating, disassociated, disassociating, failed, failing"

instance ToText VPCCidrBlockStateCode where
  toText = \case
    VCBSCAssociated -> "associated"
    VCBSCAssociating -> "associating"
    VCBSCDisassociated -> "disassociated"
    VCBSCDisassociating -> "disassociating"
    VCBSCFailed -> "failed"
    VCBSCFailing -> "failing"

instance Hashable VPCCidrBlockStateCode

instance NFData VPCCidrBlockStateCode

instance ToByteString VPCCidrBlockStateCode

instance ToQuery VPCCidrBlockStateCode

instance ToHeader VPCCidrBlockStateCode

instance FromXML VPCCidrBlockStateCode where
  parseXML = parseXMLText "VPCCidrBlockStateCode"
