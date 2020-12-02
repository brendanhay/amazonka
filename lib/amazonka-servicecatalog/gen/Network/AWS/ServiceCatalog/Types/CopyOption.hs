{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.CopyOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.CopyOption where

import Network.AWS.Prelude

data CopyOption = CopyTags
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

instance FromText CopyOption where
  parser =
    takeLowerText >>= \case
      "copytags" -> pure CopyTags
      e ->
        fromTextError $
          "Failure parsing CopyOption from value: '" <> e
            <> "'. Accepted values: copytags"

instance ToText CopyOption where
  toText = \case
    CopyTags -> "CopyTags"

instance Hashable CopyOption

instance NFData CopyOption

instance ToByteString CopyOption

instance ToQuery CopyOption

instance ToHeader CopyOption

instance ToJSON CopyOption where
  toJSON = toJSONText
