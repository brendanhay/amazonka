{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VPCTenancy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VPCTenancy where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data VPCTenancy = VTDefault
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

instance FromText VPCTenancy where
  parser =
    takeLowerText >>= \case
      "default" -> pure VTDefault
      e ->
        fromTextError $
          "Failure parsing VPCTenancy from value: '" <> e
            <> "'. Accepted values: default"

instance ToText VPCTenancy where
  toText = \case
    VTDefault -> "default"

instance Hashable VPCTenancy

instance NFData VPCTenancy

instance ToByteString VPCTenancy

instance ToQuery VPCTenancy

instance ToHeader VPCTenancy
