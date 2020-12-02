{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ServiceField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ServiceField where

import Network.AWS.Prelude

data ServiceField = SFTags
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

instance FromText ServiceField where
  parser =
    takeLowerText >>= \case
      "tags" -> pure SFTags
      e ->
        fromTextError $
          "Failure parsing ServiceField from value: '" <> e
            <> "'. Accepted values: tags"

instance ToText ServiceField where
  toText = \case
    SFTags -> "TAGS"

instance Hashable ServiceField

instance NFData ServiceField

instance ToByteString ServiceField

instance ToQuery ServiceField

instance ToHeader ServiceField

instance ToJSON ServiceField where
  toJSON = toJSONText
