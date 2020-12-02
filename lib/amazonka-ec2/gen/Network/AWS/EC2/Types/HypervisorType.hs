{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HypervisorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HypervisorType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data HypervisorType
  = HTOvm
  | HTXen
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

instance FromText HypervisorType where
  parser =
    takeLowerText >>= \case
      "ovm" -> pure HTOvm
      "xen" -> pure HTXen
      e ->
        fromTextError $
          "Failure parsing HypervisorType from value: '" <> e
            <> "'. Accepted values: ovm, xen"

instance ToText HypervisorType where
  toText = \case
    HTOvm -> "ovm"
    HTXen -> "xen"

instance Hashable HypervisorType

instance NFData HypervisorType

instance ToByteString HypervisorType

instance ToQuery HypervisorType

instance ToHeader HypervisorType

instance FromXML HypervisorType where
  parseXML = parseXMLText "HypervisorType"
