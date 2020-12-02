{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImageAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageAttributeName where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data FpgaImageAttributeName
  = FIANDescription
  | FIANLoadPermission
  | FIANName
  | FIANProductCodes
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

instance FromText FpgaImageAttributeName where
  parser =
    takeLowerText >>= \case
      "description" -> pure FIANDescription
      "loadpermission" -> pure FIANLoadPermission
      "name" -> pure FIANName
      "productcodes" -> pure FIANProductCodes
      e ->
        fromTextError $
          "Failure parsing FpgaImageAttributeName from value: '" <> e
            <> "'. Accepted values: description, loadpermission, name, productcodes"

instance ToText FpgaImageAttributeName where
  toText = \case
    FIANDescription -> "description"
    FIANLoadPermission -> "loadPermission"
    FIANName -> "name"
    FIANProductCodes -> "productCodes"

instance Hashable FpgaImageAttributeName

instance NFData FpgaImageAttributeName

instance ToByteString FpgaImageAttributeName

instance ToQuery FpgaImageAttributeName

instance ToHeader FpgaImageAttributeName
