{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VolumeAttributeName
  = VANAutoEnableIO
  | VANProductCodes
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

instance FromText VolumeAttributeName where
  parser =
    takeLowerText >>= \case
      "autoenableio" -> pure VANAutoEnableIO
      "productcodes" -> pure VANProductCodes
      e ->
        fromTextError $
          "Failure parsing VolumeAttributeName from value: '" <> e
            <> "'. Accepted values: autoenableio, productcodes"

instance ToText VolumeAttributeName where
  toText = \case
    VANAutoEnableIO -> "autoEnableIO"
    VANProductCodes -> "productCodes"

instance Hashable VolumeAttributeName

instance NFData VolumeAttributeName

instance ToByteString VolumeAttributeName

instance ToQuery VolumeAttributeName

instance ToHeader VolumeAttributeName
