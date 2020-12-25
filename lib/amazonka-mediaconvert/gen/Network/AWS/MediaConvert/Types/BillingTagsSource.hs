{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.BillingTagsSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BillingTagsSource
  ( BillingTagsSource
      ( BillingTagsSource',
        BillingTagsSourceQueue,
        BillingTagsSourcePreset,
        BillingTagsSourceJobTemplate,
        BillingTagsSourceJob,
        fromBillingTagsSource
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The tag type that AWS Billing and Cost Management will use to sort your AWS Elemental MediaConvert costs on any billing report that you set up.
newtype BillingTagsSource = BillingTagsSource'
  { fromBillingTagsSource ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern BillingTagsSourceQueue :: BillingTagsSource
pattern BillingTagsSourceQueue = BillingTagsSource' "QUEUE"

pattern BillingTagsSourcePreset :: BillingTagsSource
pattern BillingTagsSourcePreset = BillingTagsSource' "PRESET"

pattern BillingTagsSourceJobTemplate :: BillingTagsSource
pattern BillingTagsSourceJobTemplate = BillingTagsSource' "JOB_TEMPLATE"

pattern BillingTagsSourceJob :: BillingTagsSource
pattern BillingTagsSourceJob = BillingTagsSource' "JOB"

{-# COMPLETE
  BillingTagsSourceQueue,
  BillingTagsSourcePreset,
  BillingTagsSourceJobTemplate,
  BillingTagsSourceJob,
  BillingTagsSource'
  #-}
