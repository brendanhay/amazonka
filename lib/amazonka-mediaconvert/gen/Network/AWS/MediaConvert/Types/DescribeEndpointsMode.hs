{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DescribeEndpointsMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DescribeEndpointsMode where

import Network.AWS.Prelude

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
data DescribeEndpointsMode
  = DEMDefault
  | DEMGetOnly
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

instance FromText DescribeEndpointsMode where
  parser =
    takeLowerText >>= \case
      "default" -> pure DEMDefault
      "get_only" -> pure DEMGetOnly
      e ->
        fromTextError $
          "Failure parsing DescribeEndpointsMode from value: '" <> e
            <> "'. Accepted values: default, get_only"

instance ToText DescribeEndpointsMode where
  toText = \case
    DEMDefault -> "DEFAULT"
    DEMGetOnly -> "GET_ONLY"

instance Hashable DescribeEndpointsMode

instance NFData DescribeEndpointsMode

instance ToByteString DescribeEndpointsMode

instance ToQuery DescribeEndpointsMode

instance ToHeader DescribeEndpointsMode

instance ToJSON DescribeEndpointsMode where
  toJSON = toJSONText
