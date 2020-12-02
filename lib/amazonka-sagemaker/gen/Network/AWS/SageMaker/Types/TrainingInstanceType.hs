{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingInstanceType where

import Network.AWS.Prelude

data TrainingInstanceType
  = TITMl_C4_2XLarge
  | TITMl_C4_4XLarge
  | TITMl_C4_8XLarge
  | TITMl_C4_XLarge
  | TITMl_C5_18XLarge
  | TITMl_C5_2XLarge
  | TITMl_C5_4XLarge
  | TITMl_C5_9XLarge
  | TITMl_C5_XLarge
  | TITMl_C5n_18XLarge
  | TITMl_C5n_2XLarge
  | TITMl_C5n_4XLarge
  | TITMl_C5n_9XLarge
  | TITMl_C5n_XLarge
  | TITMl_G4dn_12XLarge
  | TITMl_G4dn_16XLarge
  | TITMl_G4dn_2XLarge
  | TITMl_G4dn_4XLarge
  | TITMl_G4dn_8XLarge
  | TITMl_G4dn_XLarge
  | TITMl_M4_10XLarge
  | TITMl_M4_16XLarge
  | TITMl_M4_2XLarge
  | TITMl_M4_4XLarge
  | TITMl_M4_XLarge
  | TITMl_M5_12XLarge
  | TITMl_M5_24XLarge
  | TITMl_M5_2XLarge
  | TITMl_M5_4XLarge
  | TITMl_M5_Large
  | TITMl_M5_XLarge
  | TITMl_P2_16XLarge
  | TITMl_P2_8XLarge
  | TITMl_P2_XLarge
  | TITMl_P3_16XLarge
  | TITMl_P3_2XLarge
  | TITMl_P3_8XLarge
  | TITMl_P3dn_24XLarge
  | TITMl_P4d_24XLarge
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

instance FromText TrainingInstanceType where
  parser =
    takeLowerText >>= \case
      "ml.c4.2xlarge" -> pure TITMl_C4_2XLarge
      "ml.c4.4xlarge" -> pure TITMl_C4_4XLarge
      "ml.c4.8xlarge" -> pure TITMl_C4_8XLarge
      "ml.c4.xlarge" -> pure TITMl_C4_XLarge
      "ml.c5.18xlarge" -> pure TITMl_C5_18XLarge
      "ml.c5.2xlarge" -> pure TITMl_C5_2XLarge
      "ml.c5.4xlarge" -> pure TITMl_C5_4XLarge
      "ml.c5.9xlarge" -> pure TITMl_C5_9XLarge
      "ml.c5.xlarge" -> pure TITMl_C5_XLarge
      "ml.c5n.18xlarge" -> pure TITMl_C5n_18XLarge
      "ml.c5n.2xlarge" -> pure TITMl_C5n_2XLarge
      "ml.c5n.4xlarge" -> pure TITMl_C5n_4XLarge
      "ml.c5n.9xlarge" -> pure TITMl_C5n_9XLarge
      "ml.c5n.xlarge" -> pure TITMl_C5n_XLarge
      "ml.g4dn.12xlarge" -> pure TITMl_G4dn_12XLarge
      "ml.g4dn.16xlarge" -> pure TITMl_G4dn_16XLarge
      "ml.g4dn.2xlarge" -> pure TITMl_G4dn_2XLarge
      "ml.g4dn.4xlarge" -> pure TITMl_G4dn_4XLarge
      "ml.g4dn.8xlarge" -> pure TITMl_G4dn_8XLarge
      "ml.g4dn.xlarge" -> pure TITMl_G4dn_XLarge
      "ml.m4.10xlarge" -> pure TITMl_M4_10XLarge
      "ml.m4.16xlarge" -> pure TITMl_M4_16XLarge
      "ml.m4.2xlarge" -> pure TITMl_M4_2XLarge
      "ml.m4.4xlarge" -> pure TITMl_M4_4XLarge
      "ml.m4.xlarge" -> pure TITMl_M4_XLarge
      "ml.m5.12xlarge" -> pure TITMl_M5_12XLarge
      "ml.m5.24xlarge" -> pure TITMl_M5_24XLarge
      "ml.m5.2xlarge" -> pure TITMl_M5_2XLarge
      "ml.m5.4xlarge" -> pure TITMl_M5_4XLarge
      "ml.m5.large" -> pure TITMl_M5_Large
      "ml.m5.xlarge" -> pure TITMl_M5_XLarge
      "ml.p2.16xlarge" -> pure TITMl_P2_16XLarge
      "ml.p2.8xlarge" -> pure TITMl_P2_8XLarge
      "ml.p2.xlarge" -> pure TITMl_P2_XLarge
      "ml.p3.16xlarge" -> pure TITMl_P3_16XLarge
      "ml.p3.2xlarge" -> pure TITMl_P3_2XLarge
      "ml.p3.8xlarge" -> pure TITMl_P3_8XLarge
      "ml.p3dn.24xlarge" -> pure TITMl_P3dn_24XLarge
      "ml.p4d.24xlarge" -> pure TITMl_P4d_24XLarge
      e ->
        fromTextError $
          "Failure parsing TrainingInstanceType from value: '" <> e
            <> "'. Accepted values: ml.c4.2xlarge, ml.c4.4xlarge, ml.c4.8xlarge, ml.c4.xlarge, ml.c5.18xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.xlarge, ml.c5n.18xlarge, ml.c5n.2xlarge, ml.c5n.4xlarge, ml.c5n.9xlarge, ml.c5n.xlarge, ml.g4dn.12xlarge, ml.g4dn.16xlarge, ml.g4dn.2xlarge, ml.g4dn.4xlarge, ml.g4dn.8xlarge, ml.g4dn.xlarge, ml.m4.10xlarge, ml.m4.16xlarge, ml.m4.2xlarge, ml.m4.4xlarge, ml.m4.xlarge, ml.m5.12xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.large, ml.m5.xlarge, ml.p2.16xlarge, ml.p2.8xlarge, ml.p2.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge, ml.p3dn.24xlarge, ml.p4d.24xlarge"

instance ToText TrainingInstanceType where
  toText = \case
    TITMl_C4_2XLarge -> "ml.c4.2xlarge"
    TITMl_C4_4XLarge -> "ml.c4.4xlarge"
    TITMl_C4_8XLarge -> "ml.c4.8xlarge"
    TITMl_C4_XLarge -> "ml.c4.xlarge"
    TITMl_C5_18XLarge -> "ml.c5.18xlarge"
    TITMl_C5_2XLarge -> "ml.c5.2xlarge"
    TITMl_C5_4XLarge -> "ml.c5.4xlarge"
    TITMl_C5_9XLarge -> "ml.c5.9xlarge"
    TITMl_C5_XLarge -> "ml.c5.xlarge"
    TITMl_C5n_18XLarge -> "ml.c5n.18xlarge"
    TITMl_C5n_2XLarge -> "ml.c5n.2xlarge"
    TITMl_C5n_4XLarge -> "ml.c5n.4xlarge"
    TITMl_C5n_9XLarge -> "ml.c5n.9xlarge"
    TITMl_C5n_XLarge -> "ml.c5n.xlarge"
    TITMl_G4dn_12XLarge -> "ml.g4dn.12xlarge"
    TITMl_G4dn_16XLarge -> "ml.g4dn.16xlarge"
    TITMl_G4dn_2XLarge -> "ml.g4dn.2xlarge"
    TITMl_G4dn_4XLarge -> "ml.g4dn.4xlarge"
    TITMl_G4dn_8XLarge -> "ml.g4dn.8xlarge"
    TITMl_G4dn_XLarge -> "ml.g4dn.xlarge"
    TITMl_M4_10XLarge -> "ml.m4.10xlarge"
    TITMl_M4_16XLarge -> "ml.m4.16xlarge"
    TITMl_M4_2XLarge -> "ml.m4.2xlarge"
    TITMl_M4_4XLarge -> "ml.m4.4xlarge"
    TITMl_M4_XLarge -> "ml.m4.xlarge"
    TITMl_M5_12XLarge -> "ml.m5.12xlarge"
    TITMl_M5_24XLarge -> "ml.m5.24xlarge"
    TITMl_M5_2XLarge -> "ml.m5.2xlarge"
    TITMl_M5_4XLarge -> "ml.m5.4xlarge"
    TITMl_M5_Large -> "ml.m5.large"
    TITMl_M5_XLarge -> "ml.m5.xlarge"
    TITMl_P2_16XLarge -> "ml.p2.16xlarge"
    TITMl_P2_8XLarge -> "ml.p2.8xlarge"
    TITMl_P2_XLarge -> "ml.p2.xlarge"
    TITMl_P3_16XLarge -> "ml.p3.16xlarge"
    TITMl_P3_2XLarge -> "ml.p3.2xlarge"
    TITMl_P3_8XLarge -> "ml.p3.8xlarge"
    TITMl_P3dn_24XLarge -> "ml.p3dn.24xlarge"
    TITMl_P4d_24XLarge -> "ml.p4d.24xlarge"

instance Hashable TrainingInstanceType

instance NFData TrainingInstanceType

instance ToByteString TrainingInstanceType

instance ToQuery TrainingInstanceType

instance ToHeader TrainingInstanceType

instance ToJSON TrainingInstanceType where
  toJSON = toJSONText

instance FromJSON TrainingInstanceType where
  parseJSON = parseJSONText "TrainingInstanceType"
