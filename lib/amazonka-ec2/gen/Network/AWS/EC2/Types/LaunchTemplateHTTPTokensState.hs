{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateHTTPTokensState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data LaunchTemplateHTTPTokensState
  = Optional
  | Required
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

instance FromText LaunchTemplateHTTPTokensState where
  parser =
    takeLowerText >>= \case
      "optional" -> pure Optional
      "required" -> pure Required
      e ->
        fromTextError $
          "Failure parsing LaunchTemplateHTTPTokensState from value: '" <> e
            <> "'. Accepted values: optional, required"

instance ToText LaunchTemplateHTTPTokensState where
  toText = \case
    Optional -> "optional"
    Required -> "required"

instance Hashable LaunchTemplateHTTPTokensState

instance NFData LaunchTemplateHTTPTokensState

instance ToByteString LaunchTemplateHTTPTokensState

instance ToQuery LaunchTemplateHTTPTokensState

instance ToHeader LaunchTemplateHTTPTokensState

instance FromXML LaunchTemplateHTTPTokensState where
  parseXML = parseXMLText "LaunchTemplateHTTPTokensState"
