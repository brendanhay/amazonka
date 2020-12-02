{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchSet where

import Network.AWS.Prelude

data PatchSet
  = Application
  | OS
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

instance FromText PatchSet where
  parser =
    takeLowerText >>= \case
      "application" -> pure Application
      "os" -> pure OS
      e ->
        fromTextError $
          "Failure parsing PatchSet from value: '" <> e
            <> "'. Accepted values: application, os"

instance ToText PatchSet where
  toText = \case
    Application -> "APPLICATION"
    OS -> "OS"

instance Hashable PatchSet

instance NFData PatchSet

instance ToByteString PatchSet

instance ToQuery PatchSet

instance ToHeader PatchSet

instance ToJSON PatchSet where
  toJSON = toJSONText
