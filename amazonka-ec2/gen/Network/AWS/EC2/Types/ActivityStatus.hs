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
-- Module      : Network.AWS.EC2.Types.ActivityStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ActivityStatus
  ( ActivityStatus
      ( ..,
        ActivityStatus_Error,
        ActivityStatus_Fulfilled,
        ActivityStatus_Pending_fulfillment,
        ActivityStatus_Pending_termination
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ActivityStatus = ActivityStatus'
  { fromActivityStatus ::
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

pattern ActivityStatus_Error :: ActivityStatus
pattern ActivityStatus_Error = ActivityStatus' "error"

pattern ActivityStatus_Fulfilled :: ActivityStatus
pattern ActivityStatus_Fulfilled = ActivityStatus' "fulfilled"

pattern ActivityStatus_Pending_fulfillment :: ActivityStatus
pattern ActivityStatus_Pending_fulfillment = ActivityStatus' "pending_fulfillment"

pattern ActivityStatus_Pending_termination :: ActivityStatus
pattern ActivityStatus_Pending_termination = ActivityStatus' "pending_termination"

{-# COMPLETE
  ActivityStatus_Error,
  ActivityStatus_Fulfilled,
  ActivityStatus_Pending_fulfillment,
  ActivityStatus_Pending_termination,
  ActivityStatus'
  #-}
