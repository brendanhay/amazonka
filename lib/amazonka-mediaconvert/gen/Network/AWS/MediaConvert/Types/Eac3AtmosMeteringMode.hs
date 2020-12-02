{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode where

import Network.AWS.Prelude

-- | Choose how the service meters the loudness of your audio.
data Eac3AtmosMeteringMode
  = EAMMItuBs17701
  | EAMMItuBs17702
  | EAMMItuBs17703
  | EAMMItuBs17704
  | EAMMLeqA
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

instance FromText Eac3AtmosMeteringMode where
  parser =
    takeLowerText >>= \case
      "itu_bs_1770_1" -> pure EAMMItuBs17701
      "itu_bs_1770_2" -> pure EAMMItuBs17702
      "itu_bs_1770_3" -> pure EAMMItuBs17703
      "itu_bs_1770_4" -> pure EAMMItuBs17704
      "leq_a" -> pure EAMMLeqA
      e ->
        fromTextError $
          "Failure parsing Eac3AtmosMeteringMode from value: '" <> e
            <> "'. Accepted values: itu_bs_1770_1, itu_bs_1770_2, itu_bs_1770_3, itu_bs_1770_4, leq_a"

instance ToText Eac3AtmosMeteringMode where
  toText = \case
    EAMMItuBs17701 -> "ITU_BS_1770_1"
    EAMMItuBs17702 -> "ITU_BS_1770_2"
    EAMMItuBs17703 -> "ITU_BS_1770_3"
    EAMMItuBs17704 -> "ITU_BS_1770_4"
    EAMMLeqA -> "LEQ_A"

instance Hashable Eac3AtmosMeteringMode

instance NFData Eac3AtmosMeteringMode

instance ToByteString Eac3AtmosMeteringMode

instance ToQuery Eac3AtmosMeteringMode

instance ToHeader Eac3AtmosMeteringMode

instance ToJSON Eac3AtmosMeteringMode where
  toJSON = toJSONText

instance FromJSON Eac3AtmosMeteringMode where
  parseJSON = parseJSONText "Eac3AtmosMeteringMode"
