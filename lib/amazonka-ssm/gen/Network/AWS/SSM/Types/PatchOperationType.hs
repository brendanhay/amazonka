{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchOperationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchOperationType where

import Network.AWS.Prelude

data PatchOperationType
  = Install
  | Scan
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

instance FromText PatchOperationType where
  parser =
    takeLowerText >>= \case
      "install" -> pure Install
      "scan" -> pure Scan
      e ->
        fromTextError $
          "Failure parsing PatchOperationType from value: '" <> e
            <> "'. Accepted values: install, scan"

instance ToText PatchOperationType where
  toText = \case
    Install -> "Install"
    Scan -> "Scan"

instance Hashable PatchOperationType

instance NFData PatchOperationType

instance ToByteString PatchOperationType

instance ToQuery PatchOperationType

instance ToHeader PatchOperationType

instance FromJSON PatchOperationType where
  parseJSON = parseJSONText "PatchOperationType"
