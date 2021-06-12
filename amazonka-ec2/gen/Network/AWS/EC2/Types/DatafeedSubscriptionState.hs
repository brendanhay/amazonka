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
-- Module      : Network.AWS.EC2.Types.DatafeedSubscriptionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DatafeedSubscriptionState
  ( DatafeedSubscriptionState
      ( ..,
        DatafeedSubscriptionState_Active,
        DatafeedSubscriptionState_Inactive
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype DatafeedSubscriptionState = DatafeedSubscriptionState'
  { fromDatafeedSubscriptionState ::
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

pattern DatafeedSubscriptionState_Active :: DatafeedSubscriptionState
pattern DatafeedSubscriptionState_Active = DatafeedSubscriptionState' "Active"

pattern DatafeedSubscriptionState_Inactive :: DatafeedSubscriptionState
pattern DatafeedSubscriptionState_Inactive = DatafeedSubscriptionState' "Inactive"

{-# COMPLETE
  DatafeedSubscriptionState_Active,
  DatafeedSubscriptionState_Inactive,
  DatafeedSubscriptionState'
  #-}
