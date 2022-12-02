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
-- Module      : Amazonka.Backup.Types.CopyJobState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.CopyJobState
  ( CopyJobState
      ( ..,
        CopyJobState_COMPLETED,
        CopyJobState_CREATED,
        CopyJobState_FAILED,
        CopyJobState_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CopyJobState = CopyJobState'
  { fromCopyJobState ::
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

pattern CopyJobState_COMPLETED :: CopyJobState
pattern CopyJobState_COMPLETED = CopyJobState' "COMPLETED"

pattern CopyJobState_CREATED :: CopyJobState
pattern CopyJobState_CREATED = CopyJobState' "CREATED"

pattern CopyJobState_FAILED :: CopyJobState
pattern CopyJobState_FAILED = CopyJobState' "FAILED"

pattern CopyJobState_RUNNING :: CopyJobState
pattern CopyJobState_RUNNING = CopyJobState' "RUNNING"

{-# COMPLETE
  CopyJobState_COMPLETED,
  CopyJobState_CREATED,
  CopyJobState_FAILED,
  CopyJobState_RUNNING,
  CopyJobState'
  #-}
