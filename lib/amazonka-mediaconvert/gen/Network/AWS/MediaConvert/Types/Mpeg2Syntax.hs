{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Syntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2Syntax where

import Network.AWS.Prelude

-- | Specify whether this output's video uses the D10 syntax. Keep the default value to  not use the syntax. Related settings: When you choose D10 (D_10) for your MXF  profile (profile), you must also set this value to to D10 (D_10).
data Mpeg2Syntax
  = D10
  | Default
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

instance FromText Mpeg2Syntax where
  parser =
    takeLowerText >>= \case
      "d_10" -> pure D10
      "default" -> pure Default
      e ->
        fromTextError $
          "Failure parsing Mpeg2Syntax from value: '" <> e
            <> "'. Accepted values: d_10, default"

instance ToText Mpeg2Syntax where
  toText = \case
    D10 -> "D_10"
    Default -> "DEFAULT"

instance Hashable Mpeg2Syntax

instance NFData Mpeg2Syntax

instance ToByteString Mpeg2Syntax

instance ToQuery Mpeg2Syntax

instance ToHeader Mpeg2Syntax

instance ToJSON Mpeg2Syntax where
  toJSON = toJSONText

instance FromJSON Mpeg2Syntax where
  parseJSON = parseJSONText "Mpeg2Syntax"
