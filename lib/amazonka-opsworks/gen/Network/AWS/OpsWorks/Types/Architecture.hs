{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Architecture
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Architecture where

import Network.AWS.Prelude

data Architecture
  = I386
  | X86_64
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

instance FromText Architecture where
  parser =
    takeLowerText >>= \case
      "i386" -> pure I386
      "x86_64" -> pure X86_64
      e ->
        fromTextError $
          "Failure parsing Architecture from value: '" <> e
            <> "'. Accepted values: i386, x86_64"

instance ToText Architecture where
  toText = \case
    I386 -> "i386"
    X86_64 -> "x86_64"

instance Hashable Architecture

instance NFData Architecture

instance ToByteString Architecture

instance ToQuery Architecture

instance ToHeader Architecture

instance ToJSON Architecture where
  toJSON = toJSONText

instance FromJSON Architecture where
  parseJSON = parseJSONText "Architecture"
