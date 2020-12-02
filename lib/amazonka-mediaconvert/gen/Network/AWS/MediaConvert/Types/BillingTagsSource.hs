{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BillingTagsSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BillingTagsSource where

import Network.AWS.Prelude

-- | The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
data BillingTagsSource
  = Job
  | JobTemplate
  | Preset
  | Queue
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

instance FromText BillingTagsSource where
  parser =
    takeLowerText >>= \case
      "job" -> pure Job
      "job_template" -> pure JobTemplate
      "preset" -> pure Preset
      "queue" -> pure Queue
      e ->
        fromTextError $
          "Failure parsing BillingTagsSource from value: '" <> e
            <> "'. Accepted values: job, job_template, preset, queue"

instance ToText BillingTagsSource where
  toText = \case
    Job -> "JOB"
    JobTemplate -> "JOB_TEMPLATE"
    Preset -> "PRESET"
    Queue -> "QUEUE"

instance Hashable BillingTagsSource

instance NFData BillingTagsSource

instance ToByteString BillingTagsSource

instance ToQuery BillingTagsSource

instance ToHeader BillingTagsSource

instance ToJSON BillingTagsSource where
  toJSON = toJSONText

instance FromJSON BillingTagsSource where
  parseJSON = parseJSONText "BillingTagsSource"
