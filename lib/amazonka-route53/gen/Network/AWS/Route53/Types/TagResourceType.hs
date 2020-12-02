{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TagResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.TagResourceType where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data TagResourceType
  = Healthcheck
  | Hostedzone
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

instance FromText TagResourceType where
  parser =
    takeLowerText >>= \case
      "healthcheck" -> pure Healthcheck
      "hostedzone" -> pure Hostedzone
      e ->
        fromTextError $
          "Failure parsing TagResourceType from value: '" <> e
            <> "'. Accepted values: healthcheck, hostedzone"

instance ToText TagResourceType where
  toText = \case
    Healthcheck -> "healthcheck"
    Hostedzone -> "hostedzone"

instance Hashable TagResourceType

instance NFData TagResourceType

instance ToByteString TagResourceType

instance ToQuery TagResourceType

instance ToHeader TagResourceType

instance FromXML TagResourceType where
  parseXML = parseXMLText "TagResourceType"

instance ToXML TagResourceType where
  toXML = toXMLText
