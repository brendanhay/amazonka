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
-- Module      : Network.AWS.GuardDuty.Types.FindingPublishingFrequency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingPublishingFrequency
  ( FindingPublishingFrequency
      ( ..,
        FindingPublishingFrequency_FIFTEEN_MINUTES,
        FindingPublishingFrequency_ONE_HOUR,
        FindingPublishingFrequency_SIX_HOURS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype FindingPublishingFrequency = FindingPublishingFrequency'
  { fromFindingPublishingFrequency ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern FindingPublishingFrequency_FIFTEEN_MINUTES :: FindingPublishingFrequency
pattern FindingPublishingFrequency_FIFTEEN_MINUTES = FindingPublishingFrequency' "FIFTEEN_MINUTES"

pattern FindingPublishingFrequency_ONE_HOUR :: FindingPublishingFrequency
pattern FindingPublishingFrequency_ONE_HOUR = FindingPublishingFrequency' "ONE_HOUR"

pattern FindingPublishingFrequency_SIX_HOURS :: FindingPublishingFrequency
pattern FindingPublishingFrequency_SIX_HOURS = FindingPublishingFrequency' "SIX_HOURS"

{-# COMPLETE
  FindingPublishingFrequency_FIFTEEN_MINUTES,
  FindingPublishingFrequency_ONE_HOUR,
  FindingPublishingFrequency_SIX_HOURS,
  FindingPublishingFrequency'
  #-}
