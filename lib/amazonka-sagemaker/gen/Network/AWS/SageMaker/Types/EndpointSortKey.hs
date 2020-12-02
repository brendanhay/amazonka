{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointSortKey where

import Network.AWS.Prelude

data EndpointSortKey
  = ESKCreationTime
  | ESKName
  | ESKStatus
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

instance FromText EndpointSortKey where
  parser =
    takeLowerText >>= \case
      "creationtime" -> pure ESKCreationTime
      "name" -> pure ESKName
      "status" -> pure ESKStatus
      e ->
        fromTextError $
          "Failure parsing EndpointSortKey from value: '" <> e
            <> "'. Accepted values: creationtime, name, status"

instance ToText EndpointSortKey where
  toText = \case
    ESKCreationTime -> "CreationTime"
    ESKName -> "Name"
    ESKStatus -> "Status"

instance Hashable EndpointSortKey

instance NFData EndpointSortKey

instance ToByteString EndpointSortKey

instance ToQuery EndpointSortKey

instance ToHeader EndpointSortKey

instance ToJSON EndpointSortKey where
  toJSON = toJSONText
