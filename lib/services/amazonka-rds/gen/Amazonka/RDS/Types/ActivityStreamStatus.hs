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
-- Module      : Amazonka.RDS.Types.ActivityStreamStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ActivityStreamStatus
  ( ActivityStreamStatus
      ( ..,
        ActivityStreamStatus_Started,
        ActivityStreamStatus_Starting,
        ActivityStreamStatus_Stopped,
        ActivityStreamStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActivityStreamStatus = ActivityStreamStatus'
  { fromActivityStreamStatus ::
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

pattern ActivityStreamStatus_Started :: ActivityStreamStatus
pattern ActivityStreamStatus_Started = ActivityStreamStatus' "started"

pattern ActivityStreamStatus_Starting :: ActivityStreamStatus
pattern ActivityStreamStatus_Starting = ActivityStreamStatus' "starting"

pattern ActivityStreamStatus_Stopped :: ActivityStreamStatus
pattern ActivityStreamStatus_Stopped = ActivityStreamStatus' "stopped"

pattern ActivityStreamStatus_Stopping :: ActivityStreamStatus
pattern ActivityStreamStatus_Stopping = ActivityStreamStatus' "stopping"

{-# COMPLETE
  ActivityStreamStatus_Started,
  ActivityStreamStatus_Starting,
  ActivityStreamStatus_Stopped,
  ActivityStreamStatus_Stopping,
  ActivityStreamStatus'
  #-}
