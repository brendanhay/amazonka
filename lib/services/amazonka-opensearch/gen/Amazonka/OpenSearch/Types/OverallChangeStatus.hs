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
-- Module      : Amazonka.OpenSearch.Types.OverallChangeStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OverallChangeStatus
  ( OverallChangeStatus
      ( ..,
        OverallChangeStatus_COMPLETED,
        OverallChangeStatus_FAILED,
        OverallChangeStatus_PENDING,
        OverallChangeStatus_PROCESSING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The overall status value of the domain configuration change.
newtype OverallChangeStatus = OverallChangeStatus'
  { fromOverallChangeStatus ::
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

pattern OverallChangeStatus_COMPLETED :: OverallChangeStatus
pattern OverallChangeStatus_COMPLETED = OverallChangeStatus' "COMPLETED"

pattern OverallChangeStatus_FAILED :: OverallChangeStatus
pattern OverallChangeStatus_FAILED = OverallChangeStatus' "FAILED"

pattern OverallChangeStatus_PENDING :: OverallChangeStatus
pattern OverallChangeStatus_PENDING = OverallChangeStatus' "PENDING"

pattern OverallChangeStatus_PROCESSING :: OverallChangeStatus
pattern OverallChangeStatus_PROCESSING = OverallChangeStatus' "PROCESSING"

{-# COMPLETE
  OverallChangeStatus_COMPLETED,
  OverallChangeStatus_FAILED,
  OverallChangeStatus_PENDING,
  OverallChangeStatus_PROCESSING,
  OverallChangeStatus'
  #-}
