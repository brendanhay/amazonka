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
-- Module      : Amazonka.FinSpace.Types.ChangesetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.ChangesetStatus
  ( ChangesetStatus
      ( ..,
        ChangesetStatus_COMPLETED,
        ChangesetStatus_FAILED,
        ChangesetStatus_PENDING,
        ChangesetStatus_PROCESSING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangesetStatus = ChangesetStatus'
  { fromChangesetStatus ::
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

pattern ChangesetStatus_COMPLETED :: ChangesetStatus
pattern ChangesetStatus_COMPLETED = ChangesetStatus' "COMPLETED"

pattern ChangesetStatus_FAILED :: ChangesetStatus
pattern ChangesetStatus_FAILED = ChangesetStatus' "FAILED"

pattern ChangesetStatus_PENDING :: ChangesetStatus
pattern ChangesetStatus_PENDING = ChangesetStatus' "PENDING"

pattern ChangesetStatus_PROCESSING :: ChangesetStatus
pattern ChangesetStatus_PROCESSING = ChangesetStatus' "PROCESSING"

{-# COMPLETE
  ChangesetStatus_COMPLETED,
  ChangesetStatus_FAILED,
  ChangesetStatus_PENDING,
  ChangesetStatus_PROCESSING,
  ChangesetStatus'
  #-}
