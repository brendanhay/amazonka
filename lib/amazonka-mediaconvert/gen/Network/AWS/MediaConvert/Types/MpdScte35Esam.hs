{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdScte35Esam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdScte35Esam where

import Network.AWS.Prelude

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
data MpdScte35Esam
  = MSEInsert
  | MSENone
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

instance FromText MpdScte35Esam where
  parser =
    takeLowerText >>= \case
      "insert" -> pure MSEInsert
      "none" -> pure MSENone
      e ->
        fromTextError $
          "Failure parsing MpdScte35Esam from value: '" <> e
            <> "'. Accepted values: insert, none"

instance ToText MpdScte35Esam where
  toText = \case
    MSEInsert -> "INSERT"
    MSENone -> "NONE"

instance Hashable MpdScte35Esam

instance NFData MpdScte35Esam

instance ToByteString MpdScte35Esam

instance ToQuery MpdScte35Esam

instance ToHeader MpdScte35Esam

instance ToJSON MpdScte35Esam where
  toJSON = toJSONText

instance FromJSON MpdScte35Esam where
  parseJSON = parseJSONText "MpdScte35Esam"
