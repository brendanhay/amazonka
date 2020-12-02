{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputFilterEnable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputFilterEnable where

import Network.AWS.Prelude

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
data InputFilterEnable
  = IFEAuto
  | IFEDisable
  | IFEForce
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

instance FromText InputFilterEnable where
  parser =
    takeLowerText >>= \case
      "auto" -> pure IFEAuto
      "disable" -> pure IFEDisable
      "force" -> pure IFEForce
      e ->
        fromTextError $
          "Failure parsing InputFilterEnable from value: '" <> e
            <> "'. Accepted values: auto, disable, force"

instance ToText InputFilterEnable where
  toText = \case
    IFEAuto -> "AUTO"
    IFEDisable -> "DISABLE"
    IFEForce -> "FORCE"

instance Hashable InputFilterEnable

instance NFData InputFilterEnable

instance ToByteString InputFilterEnable

instance ToQuery InputFilterEnable

instance ToHeader InputFilterEnable

instance ToJSON InputFilterEnable where
  toJSON = toJSONText

instance FromJSON InputFilterEnable where
  parseJSON = parseJSONText "InputFilterEnable"
