{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockEnabled
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockEnabled where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data ObjectLockEnabled = OLEEnabled
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

instance FromText ObjectLockEnabled where
  parser =
    takeLowerText >>= \case
      "enabled" -> pure OLEEnabled
      e ->
        fromTextError $
          "Failure parsing ObjectLockEnabled from value: '" <> e
            <> "'. Accepted values: enabled"

instance ToText ObjectLockEnabled where
  toText = \case
    OLEEnabled -> "Enabled"

instance Hashable ObjectLockEnabled

instance NFData ObjectLockEnabled

instance ToByteString ObjectLockEnabled

instance ToQuery ObjectLockEnabled

instance ToHeader ObjectLockEnabled

instance FromXML ObjectLockEnabled where
  parseXML = parseXMLText "ObjectLockEnabled"

instance ToXML ObjectLockEnabled where
  toXML = toXMLText
