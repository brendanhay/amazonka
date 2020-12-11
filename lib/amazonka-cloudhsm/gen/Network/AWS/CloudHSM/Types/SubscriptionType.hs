-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.SubscriptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.SubscriptionType
  ( SubscriptionType
      ( SubscriptionType',
        Production
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specifies the type of subscription for the HSM.
--
--
--     * __PRODUCTION__ - The HSM is being used in a production environment.
--
--
--     * __TRIAL__ - The HSM is being used in a product trial.
newtype SubscriptionType = SubscriptionType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Production :: SubscriptionType
pattern Production = SubscriptionType' "PRODUCTION"

{-# COMPLETE
  Production,
  SubscriptionType'
  #-}
