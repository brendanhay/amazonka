{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportEnvironment where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ExportEnvironment
  = Citrix
  | Microsoft
  | VMware
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

instance FromText ExportEnvironment where
  parser =
    takeLowerText >>= \case
      "citrix" -> pure Citrix
      "microsoft" -> pure Microsoft
      "vmware" -> pure VMware
      e ->
        fromTextError $
          "Failure parsing ExportEnvironment from value: '" <> e
            <> "'. Accepted values: citrix, microsoft, vmware"

instance ToText ExportEnvironment where
  toText = \case
    Citrix -> "citrix"
    Microsoft -> "microsoft"
    VMware -> "vmware"

instance Hashable ExportEnvironment

instance NFData ExportEnvironment

instance ToByteString ExportEnvironment

instance ToQuery ExportEnvironment

instance ToHeader ExportEnvironment

instance FromXML ExportEnvironment where
  parseXML = parseXMLText "ExportEnvironment"
