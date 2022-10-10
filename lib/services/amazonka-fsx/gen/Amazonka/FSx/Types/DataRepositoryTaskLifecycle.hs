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
-- Module      : Amazonka.FSx.Types.DataRepositoryTaskLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.DataRepositoryTaskLifecycle
  ( DataRepositoryTaskLifecycle
      ( ..,
        DataRepositoryTaskLifecycle_CANCELED,
        DataRepositoryTaskLifecycle_CANCELING,
        DataRepositoryTaskLifecycle_EXECUTING,
        DataRepositoryTaskLifecycle_FAILED,
        DataRepositoryTaskLifecycle_PENDING,
        DataRepositoryTaskLifecycle_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DataRepositoryTaskLifecycle = DataRepositoryTaskLifecycle'
  { fromDataRepositoryTaskLifecycle ::
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

pattern DataRepositoryTaskLifecycle_CANCELED :: DataRepositoryTaskLifecycle
pattern DataRepositoryTaskLifecycle_CANCELED = DataRepositoryTaskLifecycle' "CANCELED"

pattern DataRepositoryTaskLifecycle_CANCELING :: DataRepositoryTaskLifecycle
pattern DataRepositoryTaskLifecycle_CANCELING = DataRepositoryTaskLifecycle' "CANCELING"

pattern DataRepositoryTaskLifecycle_EXECUTING :: DataRepositoryTaskLifecycle
pattern DataRepositoryTaskLifecycle_EXECUTING = DataRepositoryTaskLifecycle' "EXECUTING"

pattern DataRepositoryTaskLifecycle_FAILED :: DataRepositoryTaskLifecycle
pattern DataRepositoryTaskLifecycle_FAILED = DataRepositoryTaskLifecycle' "FAILED"

pattern DataRepositoryTaskLifecycle_PENDING :: DataRepositoryTaskLifecycle
pattern DataRepositoryTaskLifecycle_PENDING = DataRepositoryTaskLifecycle' "PENDING"

pattern DataRepositoryTaskLifecycle_SUCCEEDED :: DataRepositoryTaskLifecycle
pattern DataRepositoryTaskLifecycle_SUCCEEDED = DataRepositoryTaskLifecycle' "SUCCEEDED"

{-# COMPLETE
  DataRepositoryTaskLifecycle_CANCELED,
  DataRepositoryTaskLifecycle_CANCELING,
  DataRepositoryTaskLifecycle_EXECUTING,
  DataRepositoryTaskLifecycle_FAILED,
  DataRepositoryTaskLifecycle_PENDING,
  DataRepositoryTaskLifecycle_SUCCEEDED,
  DataRepositoryTaskLifecycle'
  #-}
