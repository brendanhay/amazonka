{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SelfServicePortal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SelfServicePortal where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data SelfServicePortal
  = SSPDisabled
  | SSPEnabled
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

instance FromText SelfServicePortal where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure SSPDisabled
      "enabled" -> pure SSPEnabled
      e ->
        fromTextError $
          "Failure parsing SelfServicePortal from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText SelfServicePortal where
  toText = \case
    SSPDisabled -> "disabled"
    SSPEnabled -> "enabled"

instance Hashable SelfServicePortal

instance NFData SelfServicePortal

instance ToByteString SelfServicePortal

instance ToQuery SelfServicePortal

instance ToHeader SelfServicePortal
