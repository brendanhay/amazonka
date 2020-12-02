{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificateUpdateAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CACertificateUpdateAction where

import Network.AWS.Prelude

data CACertificateUpdateAction = Deactivate
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

instance FromText CACertificateUpdateAction where
  parser =
    takeLowerText >>= \case
      "deactivate" -> pure Deactivate
      e ->
        fromTextError $
          "Failure parsing CACertificateUpdateAction from value: '" <> e
            <> "'. Accepted values: deactivate"

instance ToText CACertificateUpdateAction where
  toText = \case
    Deactivate -> "DEACTIVATE"

instance Hashable CACertificateUpdateAction

instance NFData CACertificateUpdateAction

instance ToByteString CACertificateUpdateAction

instance ToQuery CACertificateUpdateAction

instance ToHeader CACertificateUpdateAction

instance ToJSON CACertificateUpdateAction where
  toJSON = toJSONText

instance FromJSON CACertificateUpdateAction where
  parseJSON = parseJSONText "CACertificateUpdateAction"
