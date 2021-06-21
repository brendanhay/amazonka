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
-- Module      : Network.AWS.SESv2.Types.SubscriptionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.SubscriptionStatus
  ( SubscriptionStatus
      ( ..,
        SubscriptionStatus_OPT_IN,
        SubscriptionStatus_OPT_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SubscriptionStatus = SubscriptionStatus'
  { fromSubscriptionStatus ::
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

pattern SubscriptionStatus_OPT_IN :: SubscriptionStatus
pattern SubscriptionStatus_OPT_IN = SubscriptionStatus' "OPT_IN"

pattern SubscriptionStatus_OPT_OUT :: SubscriptionStatus
pattern SubscriptionStatus_OPT_OUT = SubscriptionStatus' "OPT_OUT"

{-# COMPLETE
  SubscriptionStatus_OPT_IN,
  SubscriptionStatus_OPT_OUT,
  SubscriptionStatus'
  #-}
