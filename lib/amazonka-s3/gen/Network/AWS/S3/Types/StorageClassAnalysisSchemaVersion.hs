{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StorageClassAnalysisSchemaVersion where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data StorageClassAnalysisSchemaVersion = V1
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

instance FromText StorageClassAnalysisSchemaVersion where
  parser =
    takeLowerText >>= \case
      "v_1" -> pure V1
      e ->
        fromTextError $
          "Failure parsing StorageClassAnalysisSchemaVersion from value: '" <> e
            <> "'. Accepted values: v_1"

instance ToText StorageClassAnalysisSchemaVersion where
  toText = \case
    V1 -> "V_1"

instance Hashable StorageClassAnalysisSchemaVersion

instance NFData StorageClassAnalysisSchemaVersion

instance ToByteString StorageClassAnalysisSchemaVersion

instance ToQuery StorageClassAnalysisSchemaVersion

instance ToHeader StorageClassAnalysisSchemaVersion

instance FromXML StorageClassAnalysisSchemaVersion where
  parseXML = parseXMLText "StorageClassAnalysisSchemaVersion"

instance ToXML StorageClassAnalysisSchemaVersion where
  toXML = toXMLText
