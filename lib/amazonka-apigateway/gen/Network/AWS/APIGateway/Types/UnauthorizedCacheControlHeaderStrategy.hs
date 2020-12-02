{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.UnauthorizedCacheControlHeaderStrategy where

import Network.AWS.Prelude

data UnauthorizedCacheControlHeaderStrategy
  = FailWith403
  | SucceedWithResponseHeader
  | SucceedWithoutResponseHeader
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

instance FromText UnauthorizedCacheControlHeaderStrategy where
  parser =
    takeLowerText >>= \case
      "fail_with_403" -> pure FailWith403
      "succeed_with_response_header" -> pure SucceedWithResponseHeader
      "succeed_without_response_header" -> pure SucceedWithoutResponseHeader
      e ->
        fromTextError $
          "Failure parsing UnauthorizedCacheControlHeaderStrategy from value: '" <> e
            <> "'. Accepted values: fail_with_403, succeed_with_response_header, succeed_without_response_header"

instance ToText UnauthorizedCacheControlHeaderStrategy where
  toText = \case
    FailWith403 -> "FAIL_WITH_403"
    SucceedWithResponseHeader -> "SUCCEED_WITH_RESPONSE_HEADER"
    SucceedWithoutResponseHeader -> "SUCCEED_WITHOUT_RESPONSE_HEADER"

instance Hashable UnauthorizedCacheControlHeaderStrategy

instance NFData UnauthorizedCacheControlHeaderStrategy

instance ToByteString UnauthorizedCacheControlHeaderStrategy

instance ToQuery UnauthorizedCacheControlHeaderStrategy

instance ToHeader UnauthorizedCacheControlHeaderStrategy

instance FromJSON UnauthorizedCacheControlHeaderStrategy where
  parseJSON = parseJSONText "UnauthorizedCacheControlHeaderStrategy"
