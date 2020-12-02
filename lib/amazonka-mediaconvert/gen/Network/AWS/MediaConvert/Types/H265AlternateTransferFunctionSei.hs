{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265AlternateTransferFunctionSei where

import Network.AWS.Prelude

-- | Enables Alternate Transfer Function SEI message for outputs using Hybrid Log Gamma (HLG) Electro-Optical Transfer Function (EOTF).
data H265AlternateTransferFunctionSei
  = HATFSDisabled
  | HATFSEnabled
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

instance FromText H265AlternateTransferFunctionSei where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure HATFSDisabled
      "enabled" -> pure HATFSEnabled
      e ->
        fromTextError $
          "Failure parsing H265AlternateTransferFunctionSei from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText H265AlternateTransferFunctionSei where
  toText = \case
    HATFSDisabled -> "DISABLED"
    HATFSEnabled -> "ENABLED"

instance Hashable H265AlternateTransferFunctionSei

instance NFData H265AlternateTransferFunctionSei

instance ToByteString H265AlternateTransferFunctionSei

instance ToQuery H265AlternateTransferFunctionSei

instance ToHeader H265AlternateTransferFunctionSei

instance ToJSON H265AlternateTransferFunctionSei where
  toJSON = toJSONText

instance FromJSON H265AlternateTransferFunctionSei where
  parseJSON = parseJSONText "H265AlternateTransferFunctionSei"
