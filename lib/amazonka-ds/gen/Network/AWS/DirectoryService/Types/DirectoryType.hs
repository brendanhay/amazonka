{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryType where

import Network.AWS.Prelude

data DirectoryType
  = ADConnector
  | MicrosoftAD
  | SharedMicrosoftAD
  | SimpleAD
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

instance FromText DirectoryType where
  parser =
    takeLowerText >>= \case
      "adconnector" -> pure ADConnector
      "microsoftad" -> pure MicrosoftAD
      "sharedmicrosoftad" -> pure SharedMicrosoftAD
      "simplead" -> pure SimpleAD
      e ->
        fromTextError $
          "Failure parsing DirectoryType from value: '" <> e
            <> "'. Accepted values: adconnector, microsoftad, sharedmicrosoftad, simplead"

instance ToText DirectoryType where
  toText = \case
    ADConnector -> "ADConnector"
    MicrosoftAD -> "MicrosoftAD"
    SharedMicrosoftAD -> "SharedMicrosoftAD"
    SimpleAD -> "SimpleAD"

instance Hashable DirectoryType

instance NFData DirectoryType

instance ToByteString DirectoryType

instance ToQuery DirectoryType

instance ToHeader DirectoryType

instance FromJSON DirectoryType where
  parseJSON = parseJSONText "DirectoryType"
