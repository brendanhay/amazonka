{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SubnetCidrBlockStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SubnetCidrBlockStateCode where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SubnetCidrBlockStateCode
  = SCBSCAssociated
  | SCBSCAssociating
  | SCBSCDisassociated
  | SCBSCDisassociating
  | SCBSCFailed
  | SCBSCFailing
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

instance FromText SubnetCidrBlockStateCode where
  parser =
    takeLowerText >>= \case
      "associated" -> pure SCBSCAssociated
      "associating" -> pure SCBSCAssociating
      "disassociated" -> pure SCBSCDisassociated
      "disassociating" -> pure SCBSCDisassociating
      "failed" -> pure SCBSCFailed
      "failing" -> pure SCBSCFailing
      e ->
        fromTextError $
          "Failure parsing SubnetCidrBlockStateCode from value: '" <> e
            <> "'. Accepted values: associated, associating, disassociated, disassociating, failed, failing"

instance ToText SubnetCidrBlockStateCode where
  toText = \case
    SCBSCAssociated -> "associated"
    SCBSCAssociating -> "associating"
    SCBSCDisassociated -> "disassociated"
    SCBSCDisassociating -> "disassociating"
    SCBSCFailed -> "failed"
    SCBSCFailing -> "failing"

instance Hashable SubnetCidrBlockStateCode

instance NFData SubnetCidrBlockStateCode

instance ToByteString SubnetCidrBlockStateCode

instance ToQuery SubnetCidrBlockStateCode

instance ToHeader SubnetCidrBlockStateCode

instance FromXML SubnetCidrBlockStateCode where
  parseXML = parseXMLText "SubnetCidrBlockStateCode"
