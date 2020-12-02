{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PermissionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PermissionGroup where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data PermissionGroup = All
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

instance FromText PermissionGroup where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      e ->
        fromTextError $
          "Failure parsing PermissionGroup from value: '" <> e
            <> "'. Accepted values: all"

instance ToText PermissionGroup where
  toText = \case
    All -> "all"

instance Hashable PermissionGroup

instance NFData PermissionGroup

instance ToByteString PermissionGroup

instance ToQuery PermissionGroup

instance ToHeader PermissionGroup

instance FromXML PermissionGroup where
  parseXML = parseXMLText "PermissionGroup"
