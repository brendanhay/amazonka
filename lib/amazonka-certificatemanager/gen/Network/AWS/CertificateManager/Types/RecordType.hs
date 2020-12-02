{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.RecordType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.RecordType where

import Network.AWS.Prelude

data RecordType = Cname
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

instance FromText RecordType where
  parser =
    takeLowerText >>= \case
      "cname" -> pure Cname
      e ->
        fromTextError $
          "Failure parsing RecordType from value: '" <> e
            <> "'. Accepted values: cname"

instance ToText RecordType where
  toText = \case
    Cname -> "CNAME"

instance Hashable RecordType

instance NFData RecordType

instance ToByteString RecordType

instance ToQuery RecordType

instance ToHeader RecordType

instance FromJSON RecordType where
  parseJSON = parseJSONText "RecordType"
