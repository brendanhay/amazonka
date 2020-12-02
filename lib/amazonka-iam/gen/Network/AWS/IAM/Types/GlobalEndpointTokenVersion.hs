{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.GlobalEndpointTokenVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.GlobalEndpointTokenVersion where

import Network.AWS.Prelude

data GlobalEndpointTokenVersion
  = V1Token
  | V2Token
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

instance FromText GlobalEndpointTokenVersion where
  parser =
    takeLowerText >>= \case
      "v1token" -> pure V1Token
      "v2token" -> pure V2Token
      e ->
        fromTextError $
          "Failure parsing GlobalEndpointTokenVersion from value: '" <> e
            <> "'. Accepted values: v1token, v2token"

instance ToText GlobalEndpointTokenVersion where
  toText = \case
    V1Token -> "v1Token"
    V2Token -> "v2Token"

instance Hashable GlobalEndpointTokenVersion

instance NFData GlobalEndpointTokenVersion

instance ToByteString GlobalEndpointTokenVersion

instance ToQuery GlobalEndpointTokenVersion

instance ToHeader GlobalEndpointTokenVersion
