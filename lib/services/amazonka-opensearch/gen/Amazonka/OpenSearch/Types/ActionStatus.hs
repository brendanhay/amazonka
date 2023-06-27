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
-- Module      : Amazonka.OpenSearch.Types.ActionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ActionStatus
  ( ActionStatus
      ( ..,
        ActionStatus_COMPLETED,
        ActionStatus_ELIGIBLE,
        ActionStatus_FAILED,
        ActionStatus_IN_PROGRESS,
        ActionStatus_NOT_ELIGIBLE,
        ActionStatus_PENDING_UPDATE
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

pattern ActionStatus_COMPLETED :: ActionStatus
pattern ActionStatus_COMPLETED = ActionStatus' "COMPLETED"

pattern ActionStatus_ELIGIBLE :: ActionStatus
pattern ActionStatus_ELIGIBLE = ActionStatus' "ELIGIBLE"

pattern ActionStatus_FAILED :: ActionStatus
pattern ActionStatus_FAILED = ActionStatus' "FAILED"

pattern ActionStatus_IN_PROGRESS :: ActionStatus
pattern ActionStatus_IN_PROGRESS = ActionStatus' "IN_PROGRESS"

pattern ActionStatus_NOT_ELIGIBLE :: ActionStatus
pattern ActionStatus_NOT_ELIGIBLE = ActionStatus' "NOT_ELIGIBLE"

pattern ActionStatus_PENDING_UPDATE :: ActionStatus
pattern ActionStatus_PENDING_UPDATE = ActionStatus' "PENDING_UPDATE"

{-# COMPLETE
  ActionStatus_COMPLETED,
  ActionStatus_ELIGIBLE,
  ActionStatus_FAILED,
  ActionStatus_IN_PROGRESS,
  ActionStatus_NOT_ELIGIBLE,
  ActionStatus_PENDING_UPDATE,
  ActionStatus'
  #-}
