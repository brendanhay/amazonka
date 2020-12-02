{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.MFADelete
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.MFADelete where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data MFADelete
  = MDDisabled
  | MDEnabled
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

instance FromText MFADelete where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure MDDisabled
      "enabled" -> pure MDEnabled
      e ->
        fromTextError $
          "Failure parsing MFADelete from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText MFADelete where
  toText = \case
    MDDisabled -> "Disabled"
    MDEnabled -> "Enabled"

instance Hashable MFADelete

instance NFData MFADelete

instance ToByteString MFADelete

instance ToQuery MFADelete

instance ToHeader MFADelete

instance ToXML MFADelete where
  toXML = toXMLText
