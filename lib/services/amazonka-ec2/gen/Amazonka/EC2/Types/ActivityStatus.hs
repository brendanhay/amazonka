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
-- Module      : Amazonka.EC2.Types.ActivityStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ActivityStatus
  ( ActivityStatus
      ( ..,
        ActivityStatus_Error,
        ActivityStatus_Fulfilled,
        ActivityStatus_Pending_fulfillment,
        ActivityStatus_Pending_termination
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ActivityStatus = ActivityStatus'
  { fromActivityStatus ::
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
