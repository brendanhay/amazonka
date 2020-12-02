{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SnapshotAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SnapshotAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SnapshotAttributeName
  = SANCreateVolumePermission
  | SANProductCodes
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

instance FromText SnapshotAttributeName where
  parser =
    takeLowerText >>= \case
      "createvolumepermission" -> pure SANCreateVolumePermission
      "productcodes" -> pure SANProductCodes
      e ->
        fromTextError $
          "Failure parsing SnapshotAttributeName from value: '" <> e
            <> "'. Accepted values: createvolumepermission, productcodes"

instance ToText SnapshotAttributeName where
  toText = \case
    SANCreateVolumePermission -> "createVolumePermission"
    SANProductCodes -> "productCodes"

instance Hashable SnapshotAttributeName

instance NFData SnapshotAttributeName

instance ToByteString SnapshotAttributeName

instance ToQuery SnapshotAttributeName

instance ToHeader SnapshotAttributeName
