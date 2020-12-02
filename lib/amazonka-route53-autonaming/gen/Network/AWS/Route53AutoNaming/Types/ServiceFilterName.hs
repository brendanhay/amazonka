{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.ServiceFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.ServiceFilterName where

import Network.AWS.Prelude

data ServiceFilterName = NamespaceId
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

instance FromText ServiceFilterName where
  parser =
    takeLowerText >>= \case
      "namespace_id" -> pure NamespaceId
      e ->
        fromTextError $
          "Failure parsing ServiceFilterName from value: '" <> e
            <> "'. Accepted values: namespace_id"

instance ToText ServiceFilterName where
  toText = \case
    NamespaceId -> "NAMESPACE_ID"

instance Hashable ServiceFilterName

instance NFData ServiceFilterName

instance ToByteString ServiceFilterName

instance ToQuery ServiceFilterName

instance ToHeader ServiceFilterName

instance ToJSON ServiceFilterName where
  toJSON = toJSONText
