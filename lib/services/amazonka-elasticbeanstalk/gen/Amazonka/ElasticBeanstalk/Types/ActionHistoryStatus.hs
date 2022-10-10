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
-- Module      : Amazonka.ElasticBeanstalk.Types.ActionHistoryStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.ActionHistoryStatus
  ( ActionHistoryStatus
      ( ..,
        ActionHistoryStatus_Completed,
        ActionHistoryStatus_Failed,
        ActionHistoryStatus_Unknown
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ActionHistoryStatus = ActionHistoryStatus'
  { fromActionHistoryStatus ::
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

pattern ActionHistoryStatus_Completed :: ActionHistoryStatus
pattern ActionHistoryStatus_Completed = ActionHistoryStatus' "Completed"

pattern ActionHistoryStatus_Failed :: ActionHistoryStatus
pattern ActionHistoryStatus_Failed = ActionHistoryStatus' "Failed"

pattern ActionHistoryStatus_Unknown :: ActionHistoryStatus
pattern ActionHistoryStatus_Unknown = ActionHistoryStatus' "Unknown"

{-# COMPLETE
  ActionHistoryStatus_Completed,
  ActionHistoryStatus_Failed,
  ActionHistoryStatus_Unknown,
  ActionHistoryStatus'
  #-}
