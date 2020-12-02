{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryEdition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryEdition where

import Network.AWS.Prelude

data DirectoryEdition
  = Enterprise
  | Standard
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

instance FromText DirectoryEdition where
  parser =
    takeLowerText >>= \case
      "enterprise" -> pure Enterprise
      "standard" -> pure Standard
      e ->
        fromTextError $
          "Failure parsing DirectoryEdition from value: '" <> e
            <> "'. Accepted values: enterprise, standard"

instance ToText DirectoryEdition where
  toText = \case
    Enterprise -> "Enterprise"
    Standard -> "Standard"

instance Hashable DirectoryEdition

instance NFData DirectoryEdition

instance ToByteString DirectoryEdition

instance ToQuery DirectoryEdition

instance ToHeader DirectoryEdition

instance ToJSON DirectoryEdition where
  toJSON = toJSONText

instance FromJSON DirectoryEdition where
  parseJSON = parseJSONText "DirectoryEdition"
