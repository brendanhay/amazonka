{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UploadCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.UploadCategory where

import Network.AWS.Prelude

data UploadCategory
  = UCCurated
  | UCPrivate
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

instance FromText UploadCategory where
  parser =
    takeLowerText >>= \case
      "curated" -> pure UCCurated
      "private" -> pure UCPrivate
      e ->
        fromTextError $
          "Failure parsing UploadCategory from value: '" <> e
            <> "'. Accepted values: curated, private"

instance ToText UploadCategory where
  toText = \case
    UCCurated -> "CURATED"
    UCPrivate -> "PRIVATE"

instance Hashable UploadCategory

instance NFData UploadCategory

instance ToByteString UploadCategory

instance ToQuery UploadCategory

instance ToHeader UploadCategory

instance FromJSON UploadCategory where
  parseJSON = parseJSONText "UploadCategory"
