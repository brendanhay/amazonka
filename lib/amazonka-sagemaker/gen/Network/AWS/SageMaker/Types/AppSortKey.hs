{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppSortKey where

import Network.AWS.Prelude

data AppSortKey = ASKCreationTime
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

instance FromText AppSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure ASKCreationTime
      e ->
        fromTextError $
          "Failure parsing AppSortKey from value: '" <> e
            <> "'. Accepted values: creationtime"

instance ToText AppSortKey where
  toText = \case
    ASKCreationTime -> "CreationTime"

instance Hashable AppSortKey

instance NFData AppSortKey

instance ToByteString AppSortKey

instance ToQuery AppSortKey

instance ToHeader AppSortKey

instance ToJSON AppSortKey where
  toJSON = toJSONText
