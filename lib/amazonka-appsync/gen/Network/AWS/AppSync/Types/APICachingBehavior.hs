{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APICachingBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APICachingBehavior where

import Network.AWS.Prelude

data APICachingBehavior
  = FullRequestCaching
  | PerResolverCaching
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

instance FromText APICachingBehavior where
  parser =
    takeLowerText >>= \case
      "full_request_caching" -> pure FullRequestCaching
      "per_resolver_caching" -> pure PerResolverCaching
      e ->
        fromTextError $
          "Failure parsing APICachingBehavior from value: '" <> e
            <> "'. Accepted values: full_request_caching, per_resolver_caching"

instance ToText APICachingBehavior where
  toText = \case
    FullRequestCaching -> "FULL_REQUEST_CACHING"
    PerResolverCaching -> "PER_RESOLVER_CACHING"

instance Hashable APICachingBehavior

instance NFData APICachingBehavior

instance ToByteString APICachingBehavior

instance ToQuery APICachingBehavior

instance ToHeader APICachingBehavior

instance ToJSON APICachingBehavior where
  toJSON = toJSONText

instance FromJSON APICachingBehavior where
  parseJSON = parseJSONText "APICachingBehavior"
