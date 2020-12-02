{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ResetFpgaImageAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ResetFpgaImageAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ResetFpgaImageAttributeName = LoadPermission
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

instance FromText ResetFpgaImageAttributeName where
  parser =
    takeLowerText >>= \case
      "loadpermission" -> pure LoadPermission
      e ->
        fromTextError $
          "Failure parsing ResetFpgaImageAttributeName from value: '" <> e
            <> "'. Accepted values: loadpermission"

instance ToText ResetFpgaImageAttributeName where
  toText = \case
    LoadPermission -> "loadPermission"

instance Hashable ResetFpgaImageAttributeName

instance NFData ResetFpgaImageAttributeName

instance ToByteString ResetFpgaImageAttributeName

instance ToQuery ResetFpgaImageAttributeName

instance ToHeader ResetFpgaImageAttributeName
