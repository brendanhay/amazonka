{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSOptimizedSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSOptimizedSupport where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EBSOptimizedSupport
  = EOSDefault
  | EOSSupported
  | EOSUnsupported
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

instance FromText EBSOptimizedSupport where
  parser =
    takeLowerText >>= \case
      "default" -> pure EOSDefault
      "supported" -> pure EOSSupported
      "unsupported" -> pure EOSUnsupported
      e ->
        fromTextError $
          "Failure parsing EBSOptimizedSupport from value: '" <> e
            <> "'. Accepted values: default, supported, unsupported"

instance ToText EBSOptimizedSupport where
  toText = \case
    EOSDefault -> "default"
    EOSSupported -> "supported"
    EOSUnsupported -> "unsupported"

instance Hashable EBSOptimizedSupport

instance NFData EBSOptimizedSupport

instance ToByteString EBSOptimizedSupport

instance ToQuery EBSOptimizedSupport

instance ToHeader EBSOptimizedSupport

instance FromXML EBSOptimizedSupport where
  parseXML = parseXMLText "EBSOptimizedSupport"
