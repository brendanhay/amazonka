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
-- Module      : Amazonka.ElasticBeanstalk.Types.ActionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ActionStatus
  ( ActionStatus
      ( ..,
        ActionStatus_Pending,
        ActionStatus_Running,
        ActionStatus_Scheduled,
        ActionStatus_Unknown
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionStatus = ActionStatus'
  { fromActionStatus ::
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

pattern ActionStatus_Pending :: ActionStatus
pattern ActionStatus_Pending = ActionStatus' "Pending"

pattern ActionStatus_Running :: ActionStatus
pattern ActionStatus_Running = ActionStatus' "Running"

pattern ActionStatus_Scheduled :: ActionStatus
pattern ActionStatus_Scheduled = ActionStatus' "Scheduled"

pattern ActionStatus_Unknown :: ActionStatus
pattern ActionStatus_Unknown = ActionStatus' "Unknown"

{-# COMPLETE
  ActionStatus_Pending,
  ActionStatus_Running,
  ActionStatus_Scheduled,
  ActionStatus_Unknown,
  ActionStatus'
  #-}
