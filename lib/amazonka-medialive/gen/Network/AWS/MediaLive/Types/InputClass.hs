{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputClass where

import Network.AWS.Prelude

-- | A standard input has two sources and a single pipeline input only has one.
data InputClass
  = ICSinglePipeline
  | ICStandard
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

instance FromText InputClass where
  parser =
    takeLowerText >>= \case
      "single_pipeline" -> pure ICSinglePipeline
      "standard" -> pure ICStandard
      e ->
        fromTextError $
          "Failure parsing InputClass from value: '" <> e
            <> "'. Accepted values: single_pipeline, standard"

instance ToText InputClass where
  toText = \case
    ICSinglePipeline -> "SINGLE_PIPELINE"
    ICStandard -> "STANDARD"

instance Hashable InputClass

instance NFData InputClass

instance ToByteString InputClass

instance ToQuery InputClass

instance ToHeader InputClass

instance FromJSON InputClass where
  parseJSON = parseJSONText "InputClass"
