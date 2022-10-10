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
-- Module      : Amazonka.EC2.Types.DatafeedSubscriptionState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.DatafeedSubscriptionState
  ( DatafeedSubscriptionState
      ( ..,
        DatafeedSubscriptionState_Active,
        DatafeedSubscriptionState_Inactive
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype DatafeedSubscriptionState = DatafeedSubscriptionState'
  { fromDatafeedSubscriptionState ::
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

pattern DatafeedSubscriptionState_Active :: DatafeedSubscriptionState
pattern DatafeedSubscriptionState_Active = DatafeedSubscriptionState' "Active"

pattern DatafeedSubscriptionState_Inactive :: DatafeedSubscriptionState
pattern DatafeedSubscriptionState_Inactive = DatafeedSubscriptionState' "Inactive"

{-# COMPLETE
  DatafeedSubscriptionState_Active,
  DatafeedSubscriptionState_Inactive,
  DatafeedSubscriptionState'
  #-}
