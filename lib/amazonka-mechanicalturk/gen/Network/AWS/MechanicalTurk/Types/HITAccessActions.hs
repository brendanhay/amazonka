{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITAccessActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITAccessActions where

import Network.AWS.Prelude

data HITAccessActions
  = Accept
  | DiscoverPreviewAndAccept
  | PreviewAndAccept
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

instance FromText HITAccessActions where
  parser =
    takeLowerText >>= \case
      "accept" -> pure Accept
      "discoverpreviewandaccept" -> pure DiscoverPreviewAndAccept
      "previewandaccept" -> pure PreviewAndAccept
      e ->
        fromTextError $
          "Failure parsing HITAccessActions from value: '" <> e
            <> "'. Accepted values: accept, discoverpreviewandaccept, previewandaccept"

instance ToText HITAccessActions where
  toText = \case
    Accept -> "Accept"
    DiscoverPreviewAndAccept -> "DiscoverPreviewAndAccept"
    PreviewAndAccept -> "PreviewAndAccept"

instance Hashable HITAccessActions

instance NFData HITAccessActions

instance ToByteString HITAccessActions

instance ToQuery HITAccessActions

instance ToHeader HITAccessActions

instance ToJSON HITAccessActions where
  toJSON = toJSONText

instance FromJSON HITAccessActions where
  parseJSON = parseJSONText "HITAccessActions"
