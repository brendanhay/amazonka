{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.SubscriptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.SubscriptionType
  ( SubscriptionType
      ( SubscriptionType',
        SubscriptionTypeSns,
        SubscriptionTypeEmail,
        fromSubscriptionType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The subscription type of the subscriber. It can be SMS or EMAIL.
newtype SubscriptionType = SubscriptionType'
  { fromSubscriptionType ::
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

pattern SubscriptionTypeSns :: SubscriptionType
pattern SubscriptionTypeSns = SubscriptionType' "SNS"

pattern SubscriptionTypeEmail :: SubscriptionType
pattern SubscriptionTypeEmail = SubscriptionType' "EMAIL"

{-# COMPLETE
  SubscriptionTypeSns,
  SubscriptionTypeEmail,
  SubscriptionType'
  #-}
