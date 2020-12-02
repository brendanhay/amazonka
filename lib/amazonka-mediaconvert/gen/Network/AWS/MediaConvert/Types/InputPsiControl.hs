{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputPsiControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputPsiControl where

import Network.AWS.Prelude

-- | Set PSI control (InputPsiControl) for transport stream inputs to specify which data the demux process to scans. * Ignore PSI - Scan all PIDs for audio and video. * Use PSI - Scan only PSI data.
data InputPsiControl
  = IgnorePsi
  | UsePsi
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

instance FromText InputPsiControl where
  parser =
    takeLowerText >>= \case
      "ignore_psi" -> pure IgnorePsi
      "use_psi" -> pure UsePsi
      e ->
        fromTextError $
          "Failure parsing InputPsiControl from value: '" <> e
            <> "'. Accepted values: ignore_psi, use_psi"

instance ToText InputPsiControl where
  toText = \case
    IgnorePsi -> "IGNORE_PSI"
    UsePsi -> "USE_PSI"

instance Hashable InputPsiControl

instance NFData InputPsiControl

instance ToByteString InputPsiControl

instance ToQuery InputPsiControl

instance ToHeader InputPsiControl

instance ToJSON InputPsiControl where
  toJSON = toJSONText

instance FromJSON InputPsiControl where
  parseJSON = parseJSONText "InputPsiControl"
