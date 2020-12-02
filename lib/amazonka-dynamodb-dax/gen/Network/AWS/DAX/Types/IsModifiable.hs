{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.IsModifiable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.IsModifiable where

import Network.AWS.Prelude

data IsModifiable
  = Conditional
  | False'
  | True'
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

instance FromText IsModifiable where
  parser =
    takeLowerText >>= \case
      "conditional" -> pure Conditional
      "false" -> pure False'
      "true" -> pure True'
      e ->
        fromTextError $
          "Failure parsing IsModifiable from value: '" <> e
            <> "'. Accepted values: conditional, false, true"

instance ToText IsModifiable where
  toText = \case
    Conditional -> "CONDITIONAL"
    False' -> "FALSE"
    True' -> "TRUE"

instance Hashable IsModifiable

instance NFData IsModifiable

instance ToByteString IsModifiable

instance ToQuery IsModifiable

instance ToHeader IsModifiable

instance FromJSON IsModifiable where
  parseJSON = parseJSONText "IsModifiable"
