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
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContributorInsightsStatus
  ( ContributorInsightsStatus
      ( ..,
        ContributorInsightsStatus_DISABLED,
        ContributorInsightsStatus_DISABLING,
        ContributorInsightsStatus_ENABLED,
        ContributorInsightsStatus_ENABLING,
        ContributorInsightsStatus_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ContributorInsightsStatus = ContributorInsightsStatus'
  { fromContributorInsightsStatus ::
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

pattern ContributorInsightsStatus_DISABLED :: ContributorInsightsStatus
pattern ContributorInsightsStatus_DISABLED = ContributorInsightsStatus' "DISABLED"

pattern ContributorInsightsStatus_DISABLING :: ContributorInsightsStatus
pattern ContributorInsightsStatus_DISABLING = ContributorInsightsStatus' "DISABLING"

pattern ContributorInsightsStatus_ENABLED :: ContributorInsightsStatus
pattern ContributorInsightsStatus_ENABLED = ContributorInsightsStatus' "ENABLED"

pattern ContributorInsightsStatus_ENABLING :: ContributorInsightsStatus
pattern ContributorInsightsStatus_ENABLING = ContributorInsightsStatus' "ENABLING"

pattern ContributorInsightsStatus_FAILED :: ContributorInsightsStatus
pattern ContributorInsightsStatus_FAILED = ContributorInsightsStatus' "FAILED"

{-# COMPLETE
  ContributorInsightsStatus_DISABLED,
  ContributorInsightsStatus_DISABLING,
  ContributorInsightsStatus_ENABLED,
  ContributorInsightsStatus_ENABLING,
  ContributorInsightsStatus_FAILED,
  ContributorInsightsStatus'
  #-}
