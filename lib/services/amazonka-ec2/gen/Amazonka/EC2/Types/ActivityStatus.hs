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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ActivityStatus = ActivityStatus'
  { fromActivityStatus ::
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
