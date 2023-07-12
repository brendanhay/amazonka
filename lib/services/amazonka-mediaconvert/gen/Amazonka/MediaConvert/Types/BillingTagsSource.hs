{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaConvert.Types.BillingTagsSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BillingTagsSource
  ( BillingTagsSource
      ( ..,
        BillingTagsSource_JOB,
        BillingTagsSource_JOB_TEMPLATE,
        BillingTagsSource_PRESET,
        BillingTagsSource_QUEUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The tag type that AWS Billing and Cost Management will use to sort your
-- AWS Elemental MediaConvert costs on any billing report that you set up.
newtype BillingTagsSource = BillingTagsSource'
  { fromBillingTagsSource ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern BillingTagsSource_JOB :: BillingTagsSource
pattern BillingTagsSource_JOB = BillingTagsSource' "JOB"

pattern BillingTagsSource_JOB_TEMPLATE :: BillingTagsSource
pattern BillingTagsSource_JOB_TEMPLATE = BillingTagsSource' "JOB_TEMPLATE"

pattern BillingTagsSource_PRESET :: BillingTagsSource
pattern BillingTagsSource_PRESET = BillingTagsSource' "PRESET"

pattern BillingTagsSource_QUEUE :: BillingTagsSource
pattern BillingTagsSource_QUEUE = BillingTagsSource' "QUEUE"

{-# COMPLETE
  BillingTagsSource_JOB,
  BillingTagsSource_JOB_TEMPLATE,
  BillingTagsSource_PRESET,
  BillingTagsSource_QUEUE,
  BillingTagsSource'
  #-}
