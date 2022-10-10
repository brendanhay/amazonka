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
-- Module      : Amazonka.FMS.Types.MarketplaceSubscriptionOnboardingStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.MarketplaceSubscriptionOnboardingStatus
  ( MarketplaceSubscriptionOnboardingStatus
      ( ..,
        MarketplaceSubscriptionOnboardingStatus_COMPLETE,
        MarketplaceSubscriptionOnboardingStatus_NOT_COMPLETE,
        MarketplaceSubscriptionOnboardingStatus_NO_SUBSCRIPTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MarketplaceSubscriptionOnboardingStatus = MarketplaceSubscriptionOnboardingStatus'
  { fromMarketplaceSubscriptionOnboardingStatus ::
      Core.Text
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

pattern MarketplaceSubscriptionOnboardingStatus_COMPLETE :: MarketplaceSubscriptionOnboardingStatus
pattern MarketplaceSubscriptionOnboardingStatus_COMPLETE = MarketplaceSubscriptionOnboardingStatus' "COMPLETE"

pattern MarketplaceSubscriptionOnboardingStatus_NOT_COMPLETE :: MarketplaceSubscriptionOnboardingStatus
pattern MarketplaceSubscriptionOnboardingStatus_NOT_COMPLETE = MarketplaceSubscriptionOnboardingStatus' "NOT_COMPLETE"

pattern MarketplaceSubscriptionOnboardingStatus_NO_SUBSCRIPTION :: MarketplaceSubscriptionOnboardingStatus
pattern MarketplaceSubscriptionOnboardingStatus_NO_SUBSCRIPTION = MarketplaceSubscriptionOnboardingStatus' "NO_SUBSCRIPTION"

{-# COMPLETE
  MarketplaceSubscriptionOnboardingStatus_COMPLETE,
  MarketplaceSubscriptionOnboardingStatus_NOT_COMPLETE,
  MarketplaceSubscriptionOnboardingStatus_NO_SUBSCRIPTION,
  MarketplaceSubscriptionOnboardingStatus'
  #-}
