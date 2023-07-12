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
-- Module      : Amazonka.Proton.Types.RepositorySyncStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.RepositorySyncStatus
  ( RepositorySyncStatus
      ( ..,
        RepositorySyncStatus_FAILED,
        RepositorySyncStatus_INITIATED,
        RepositorySyncStatus_IN_PROGRESS,
        RepositorySyncStatus_QUEUED,
        RepositorySyncStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RepositorySyncStatus = RepositorySyncStatus'
  { fromRepositorySyncStatus ::
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

pattern RepositorySyncStatus_FAILED :: RepositorySyncStatus
pattern RepositorySyncStatus_FAILED = RepositorySyncStatus' "FAILED"

pattern RepositorySyncStatus_INITIATED :: RepositorySyncStatus
pattern RepositorySyncStatus_INITIATED = RepositorySyncStatus' "INITIATED"

pattern RepositorySyncStatus_IN_PROGRESS :: RepositorySyncStatus
pattern RepositorySyncStatus_IN_PROGRESS = RepositorySyncStatus' "IN_PROGRESS"

pattern RepositorySyncStatus_QUEUED :: RepositorySyncStatus
pattern RepositorySyncStatus_QUEUED = RepositorySyncStatus' "QUEUED"

pattern RepositorySyncStatus_SUCCEEDED :: RepositorySyncStatus
pattern RepositorySyncStatus_SUCCEEDED = RepositorySyncStatus' "SUCCEEDED"

{-# COMPLETE
  RepositorySyncStatus_FAILED,
  RepositorySyncStatus_INITIATED,
  RepositorySyncStatus_IN_PROGRESS,
  RepositorySyncStatus_QUEUED,
  RepositorySyncStatus_SUCCEEDED,
  RepositorySyncStatus'
  #-}
