{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockRetentionMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockRetentionMode where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectLockRetentionMode
  = OLRMCompliance
  | OLRMGovernance
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

instance FromText ObjectLockRetentionMode where
  parser =
    takeLowerText >>= \case
      "compliance" -> pure OLRMCompliance
      "governance" -> pure OLRMGovernance
      e ->
        fromTextError $
          "Failure parsing ObjectLockRetentionMode from value: '" <> e
            <> "'. Accepted values: compliance, governance"

instance ToText ObjectLockRetentionMode where
  toText = \case
    OLRMCompliance -> "COMPLIANCE"
    OLRMGovernance -> "GOVERNANCE"

instance Hashable ObjectLockRetentionMode

instance NFData ObjectLockRetentionMode

instance ToByteString ObjectLockRetentionMode

instance ToQuery ObjectLockRetentionMode

instance ToHeader ObjectLockRetentionMode

instance FromXML ObjectLockRetentionMode where
  parseXML = parseXMLText "ObjectLockRetentionMode"

instance ToXML ObjectLockRetentionMode where
  toXML = toXMLText
