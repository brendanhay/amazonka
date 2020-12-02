{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OrderKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OrderKey where

import Network.AWS.Prelude

data OrderKey
  = OKAscending
  | OKDescending
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

instance FromText OrderKey where
  parser =
    takeLowerText >>= \case
      "ascending" -> pure OKAscending
      "descending" -> pure OKDescending
      e ->
        fromTextError $
          "Failure parsing OrderKey from value: '" <> e
            <> "'. Accepted values: ascending, descending"

instance ToText OrderKey where
  toText = \case
    OKAscending -> "Ascending"
    OKDescending -> "Descending"

instance Hashable OrderKey

instance NFData OrderKey

instance ToByteString OrderKey

instance ToQuery OrderKey

instance ToHeader OrderKey

instance ToJSON OrderKey where
  toJSON = toJSONText
