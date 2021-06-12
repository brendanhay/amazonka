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
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContributorInsightsAction
  ( ContributorInsightsAction
      ( ..,
        ContributorInsightsAction_DISABLE,
        ContributorInsightsAction_ENABLE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ContributorInsightsAction = ContributorInsightsAction'
  { fromContributorInsightsAction ::
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

pattern ContributorInsightsAction_DISABLE :: ContributorInsightsAction
pattern ContributorInsightsAction_DISABLE = ContributorInsightsAction' "DISABLE"

pattern ContributorInsightsAction_ENABLE :: ContributorInsightsAction
pattern ContributorInsightsAction_ENABLE = ContributorInsightsAction' "ENABLE"

{-# COMPLETE
  ContributorInsightsAction_DISABLE,
  ContributorInsightsAction_ENABLE,
  ContributorInsightsAction'
  #-}
