{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RadiusAuthenticationProtocol where

import Network.AWS.Prelude

data RadiusAuthenticationProtocol
  = Chap
  | MsCHAPV1
  | MsCHAPV2
  | Pap
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

instance FromText RadiusAuthenticationProtocol where
  parser =
    takeLowerText >>= \case
      "chap" -> pure Chap
      "ms-chapv1" -> pure MsCHAPV1
      "ms-chapv2" -> pure MsCHAPV2
      "pap" -> pure Pap
      e ->
        fromTextError $
          "Failure parsing RadiusAuthenticationProtocol from value: '" <> e
            <> "'. Accepted values: chap, ms-chapv1, ms-chapv2, pap"

instance ToText RadiusAuthenticationProtocol where
  toText = \case
    Chap -> "CHAP"
    MsCHAPV1 -> "MS-CHAPv1"
    MsCHAPV2 -> "MS-CHAPv2"
    Pap -> "PAP"

instance Hashable RadiusAuthenticationProtocol

instance NFData RadiusAuthenticationProtocol

instance ToByteString RadiusAuthenticationProtocol

instance ToQuery RadiusAuthenticationProtocol

instance ToHeader RadiusAuthenticationProtocol

instance ToJSON RadiusAuthenticationProtocol where
  toJSON = toJSONText

instance FromJSON RadiusAuthenticationProtocol where
  parseJSON = parseJSONText "RadiusAuthenticationProtocol"
