{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.MemberPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.MemberPermissions where

import Network.AWS.Prelude

data MemberPermissions
  = MPReadOnly
  | MPReadWrite
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

instance FromText MemberPermissions where
  parser =
    takeLowerText >>= \case
      "read-only" -> pure MPReadOnly
      "read-write" -> pure MPReadWrite
      e ->
        fromTextError $
          "Failure parsing MemberPermissions from value: '" <> e
            <> "'. Accepted values: read-only, read-write"

instance ToText MemberPermissions where
  toText = \case
    MPReadOnly -> "read-only"
    MPReadWrite -> "read-write"

instance Hashable MemberPermissions

instance NFData MemberPermissions

instance ToByteString MemberPermissions

instance ToQuery MemberPermissions

instance ToHeader MemberPermissions

instance ToJSON MemberPermissions where
  toJSON = toJSONText
