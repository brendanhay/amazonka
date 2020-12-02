{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ModificationResourceEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ModificationResourceEnum where

import Network.AWS.Prelude

data ModificationResourceEnum
  = ComputeType
  | RootVolume
  | UserVolume
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

instance FromText ModificationResourceEnum where
  parser =
    takeLowerText >>= \case
      "compute_type" -> pure ComputeType
      "root_volume" -> pure RootVolume
      "user_volume" -> pure UserVolume
      e ->
        fromTextError $
          "Failure parsing ModificationResourceEnum from value: '" <> e
            <> "'. Accepted values: compute_type, root_volume, user_volume"

instance ToText ModificationResourceEnum where
  toText = \case
    ComputeType -> "COMPUTE_TYPE"
    RootVolume -> "ROOT_VOLUME"
    UserVolume -> "USER_VOLUME"

instance Hashable ModificationResourceEnum

instance NFData ModificationResourceEnum

instance ToByteString ModificationResourceEnum

instance ToQuery ModificationResourceEnum

instance ToHeader ModificationResourceEnum

instance FromJSON ModificationResourceEnum where
  parseJSON = parseJSONText "ModificationResourceEnum"
