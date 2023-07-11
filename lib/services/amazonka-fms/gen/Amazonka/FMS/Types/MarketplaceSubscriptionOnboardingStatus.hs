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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MarketplaceSubscriptionOnboardingStatus = MarketplaceSubscriptionOnboardingStatus'
  { fromMarketplaceSubscriptionOnboardingStatus ::
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
