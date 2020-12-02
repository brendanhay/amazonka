{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.CaseSensitivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.CaseSensitivity where

import Network.AWS.Prelude

data CaseSensitivity
  = CaseSensitive
  | ClientSpecified
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

instance FromText CaseSensitivity where
  parser =
    takeLowerText >>= \case
      "casesensitive" -> pure CaseSensitive
      "clientspecified" -> pure ClientSpecified
      e ->
        fromTextError $
          "Failure parsing CaseSensitivity from value: '" <> e
            <> "'. Accepted values: casesensitive, clientspecified"

instance ToText CaseSensitivity where
  toText = \case
    CaseSensitive -> "CaseSensitive"
    ClientSpecified -> "ClientSpecified"

instance Hashable CaseSensitivity

instance NFData CaseSensitivity

instance ToByteString CaseSensitivity

instance ToQuery CaseSensitivity

instance ToHeader CaseSensitivity

instance ToJSON CaseSensitivity where
  toJSON = toJSONText

instance FromJSON CaseSensitivity where
  parseJSON = parseJSONText "CaseSensitivity"
