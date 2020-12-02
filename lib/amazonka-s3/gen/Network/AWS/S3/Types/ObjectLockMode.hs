{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockMode where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectLockMode
  = Compliance
  | Governance
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

instance FromText ObjectLockMode where
  parser =
    takeLowerText >>= \case
      "compliance" -> pure Compliance
      "governance" -> pure Governance
      e ->
        fromTextError $
          "Failure parsing ObjectLockMode from value: '" <> e
            <> "'. Accepted values: compliance, governance"

instance ToText ObjectLockMode where
  toText = \case
    Compliance -> "COMPLIANCE"
    Governance -> "GOVERNANCE"

instance Hashable ObjectLockMode

instance NFData ObjectLockMode

instance ToByteString ObjectLockMode

instance ToQuery ObjectLockMode

instance ToHeader ObjectLockMode

instance FromXML ObjectLockMode where
  parseXML = parseXMLText "ObjectLockMode"

instance ToXML ObjectLockMode where
  toXML = toXMLText
