{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppInstanceType where

import Network.AWS.Prelude

data AppInstanceType
  = Ml_C5_12XLarge
  | Ml_C5_18XLarge
  | Ml_C5_24XLarge
  | Ml_C5_2XLarge
  | Ml_C5_4XLarge
  | Ml_C5_9XLarge
  | Ml_C5_Large
  | Ml_C5_XLarge
  | Ml_G4dn_12XLarge
  | Ml_G4dn_16XLarge
  | Ml_G4dn_2XLarge
  | Ml_G4dn_4XLarge
  | Ml_G4dn_8XLarge
  | Ml_G4dn_XLarge
  | Ml_M5_12XLarge
  | Ml_M5_16XLarge
  | Ml_M5_24XLarge
  | Ml_M5_2XLarge
  | Ml_M5_4XLarge
  | Ml_M5_8XLarge
  | Ml_M5_Large
  | Ml_M5_XLarge
  | Ml_P3_16XLarge
  | Ml_P3_2XLarge
  | Ml_P3_8XLarge
  | Ml_T3_2XLarge
  | Ml_T3_Large
  | Ml_T3_Medium
  | Ml_T3_Micro
  | Ml_T3_Small
  | Ml_T3_XLarge
  | System
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

instance FromText AppInstanceType where
  parser =
    takeLowerText >>= \case
      "ml.c5.12xlarge" -> pure Ml_C5_12XLarge
      "ml.c5.18xlarge" -> pure Ml_C5_18XLarge
      "ml.c5.24xlarge" -> pure Ml_C5_24XLarge
      "ml.c5.2xlarge" -> pure Ml_C5_2XLarge
      "ml.c5.4xlarge" -> pure Ml_C5_4XLarge
      "ml.c5.9xlarge" -> pure Ml_C5_9XLarge
      "ml.c5.large" -> pure Ml_C5_Large
      "ml.c5.xlarge" -> pure Ml_C5_XLarge
      "ml.g4dn.12xlarge" -> pure Ml_G4dn_12XLarge
      "ml.g4dn.16xlarge" -> pure Ml_G4dn_16XLarge
      "ml.g4dn.2xlarge" -> pure Ml_G4dn_2XLarge
      "ml.g4dn.4xlarge" -> pure Ml_G4dn_4XLarge
      "ml.g4dn.8xlarge" -> pure Ml_G4dn_8XLarge
      "ml.g4dn.xlarge" -> pure Ml_G4dn_XLarge
      "ml.m5.12xlarge" -> pure Ml_M5_12XLarge
      "ml.m5.16xlarge" -> pure Ml_M5_16XLarge
      "ml.m5.24xlarge" -> pure Ml_M5_24XLarge
      "ml.m5.2xlarge" -> pure Ml_M5_2XLarge
      "ml.m5.4xlarge" -> pure Ml_M5_4XLarge
      "ml.m5.8xlarge" -> pure Ml_M5_8XLarge
      "ml.m5.large" -> pure Ml_M5_Large
      "ml.m5.xlarge" -> pure Ml_M5_XLarge
      "ml.p3.16xlarge" -> pure Ml_P3_16XLarge
      "ml.p3.2xlarge" -> pure Ml_P3_2XLarge
      "ml.p3.8xlarge" -> pure Ml_P3_8XLarge
      "ml.t3.2xlarge" -> pure Ml_T3_2XLarge
      "ml.t3.large" -> pure Ml_T3_Large
      "ml.t3.medium" -> pure Ml_T3_Medium
      "ml.t3.micro" -> pure Ml_T3_Micro
      "ml.t3.small" -> pure Ml_T3_Small
      "ml.t3.xlarge" -> pure Ml_T3_XLarge
      "system" -> pure System
      e ->
        fromTextError $
          "Failure parsing AppInstanceType from value: '" <> e
            <> "'. Accepted values: ml.c5.12xlarge, ml.c5.18xlarge, ml.c5.24xlarge, ml.c5.2xlarge, ml.c5.4xlarge, ml.c5.9xlarge, ml.c5.large, ml.c5.xlarge, ml.g4dn.12xlarge, ml.g4dn.16xlarge, ml.g4dn.2xlarge, ml.g4dn.4xlarge, ml.g4dn.8xlarge, ml.g4dn.xlarge, ml.m5.12xlarge, ml.m5.16xlarge, ml.m5.24xlarge, ml.m5.2xlarge, ml.m5.4xlarge, ml.m5.8xlarge, ml.m5.large, ml.m5.xlarge, ml.p3.16xlarge, ml.p3.2xlarge, ml.p3.8xlarge, ml.t3.2xlarge, ml.t3.large, ml.t3.medium, ml.t3.micro, ml.t3.small, ml.t3.xlarge, system"

instance ToText AppInstanceType where
  toText = \case
    Ml_C5_12XLarge -> "ml.c5.12xlarge"
    Ml_C5_18XLarge -> "ml.c5.18xlarge"
    Ml_C5_24XLarge -> "ml.c5.24xlarge"
    Ml_C5_2XLarge -> "ml.c5.2xlarge"
    Ml_C5_4XLarge -> "ml.c5.4xlarge"
    Ml_C5_9XLarge -> "ml.c5.9xlarge"
    Ml_C5_Large -> "ml.c5.large"
    Ml_C5_XLarge -> "ml.c5.xlarge"
    Ml_G4dn_12XLarge -> "ml.g4dn.12xlarge"
    Ml_G4dn_16XLarge -> "ml.g4dn.16xlarge"
    Ml_G4dn_2XLarge -> "ml.g4dn.2xlarge"
    Ml_G4dn_4XLarge -> "ml.g4dn.4xlarge"
    Ml_G4dn_8XLarge -> "ml.g4dn.8xlarge"
    Ml_G4dn_XLarge -> "ml.g4dn.xlarge"
    Ml_M5_12XLarge -> "ml.m5.12xlarge"
    Ml_M5_16XLarge -> "ml.m5.16xlarge"
    Ml_M5_24XLarge -> "ml.m5.24xlarge"
    Ml_M5_2XLarge -> "ml.m5.2xlarge"
    Ml_M5_4XLarge -> "ml.m5.4xlarge"
    Ml_M5_8XLarge -> "ml.m5.8xlarge"
    Ml_M5_Large -> "ml.m5.large"
    Ml_M5_XLarge -> "ml.m5.xlarge"
    Ml_P3_16XLarge -> "ml.p3.16xlarge"
    Ml_P3_2XLarge -> "ml.p3.2xlarge"
    Ml_P3_8XLarge -> "ml.p3.8xlarge"
    Ml_T3_2XLarge -> "ml.t3.2xlarge"
    Ml_T3_Large -> "ml.t3.large"
    Ml_T3_Medium -> "ml.t3.medium"
    Ml_T3_Micro -> "ml.t3.micro"
    Ml_T3_Small -> "ml.t3.small"
    Ml_T3_XLarge -> "ml.t3.xlarge"
    System -> "system"

instance Hashable AppInstanceType

instance NFData AppInstanceType

instance ToByteString AppInstanceType

instance ToQuery AppInstanceType

instance ToHeader AppInstanceType

instance ToJSON AppInstanceType where
  toJSON = toJSONText

instance FromJSON AppInstanceType where
  parseJSON = parseJSONText "AppInstanceType"
