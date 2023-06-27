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
-- Module      : Amazonka.SecurityLake.Types.DataLakeStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeStatus
  ( DataLakeStatus
      ( ..,
        DataLakeStatus_COMPLETED,
        DataLakeStatus_FAILED,
        DataLakeStatus_INITIALIZED,
        DataLakeStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataLakeStatus = DataLakeStatus'
  { fromDataLakeStatus ::
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

pattern DataLakeStatus_COMPLETED :: DataLakeStatus
pattern DataLakeStatus_COMPLETED = DataLakeStatus' "COMPLETED"

pattern DataLakeStatus_FAILED :: DataLakeStatus
pattern DataLakeStatus_FAILED = DataLakeStatus' "FAILED"

pattern DataLakeStatus_INITIALIZED :: DataLakeStatus
pattern DataLakeStatus_INITIALIZED = DataLakeStatus' "INITIALIZED"

pattern DataLakeStatus_PENDING :: DataLakeStatus
pattern DataLakeStatus_PENDING = DataLakeStatus' "PENDING"

{-# COMPLETE
  DataLakeStatus_COMPLETED,
  DataLakeStatus_FAILED,
  DataLakeStatus_INITIALIZED,
  DataLakeStatus_PENDING,
  DataLakeStatus'
  #-}
