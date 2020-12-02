{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.PerformanceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.PerformanceMode where

import Network.AWS.Prelude

data PerformanceMode
  = GeneralPurpose
  | MaxIO
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

instance FromText PerformanceMode where
  parser =
    takeLowerText >>= \case
      "generalpurpose" -> pure GeneralPurpose
      "maxio" -> pure MaxIO
      e ->
        fromTextError $
          "Failure parsing PerformanceMode from value: '" <> e
            <> "'. Accepted values: generalpurpose, maxio"

instance ToText PerformanceMode where
  toText = \case
    GeneralPurpose -> "generalPurpose"
    MaxIO -> "maxIO"

instance Hashable PerformanceMode

instance NFData PerformanceMode

instance ToByteString PerformanceMode

instance ToQuery PerformanceMode

instance ToHeader PerformanceMode

instance ToJSON PerformanceMode where
  toJSON = toJSONText

instance FromJSON PerformanceMode where
  parseJSON = parseJSONText "PerformanceMode"
