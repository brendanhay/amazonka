{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResetImageAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResetImageAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ResetImageAttributeName = RIANLaunchPermission
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

instance FromText ResetImageAttributeName where
  parser =
    takeLowerText >>= \case
      "launchpermission" -> pure RIANLaunchPermission
      e ->
        fromTextError $
          "Failure parsing ResetImageAttributeName from value: '" <> e
            <> "'. Accepted values: launchpermission"

instance ToText ResetImageAttributeName where
  toText = \case
    RIANLaunchPermission -> "launchPermission"

instance Hashable ResetImageAttributeName

instance NFData ResetImageAttributeName

instance ToByteString ResetImageAttributeName

instance ToQuery ResetImageAttributeName

instance ToHeader ResetImageAttributeName
