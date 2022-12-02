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
-- Module      : Amazonka.Kendra.Types.DataSourceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceStatus
  ( DataSourceStatus
      ( ..,
        DataSourceStatus_ACTIVE,
        DataSourceStatus_CREATING,
        DataSourceStatus_DELETING,
        DataSourceStatus_FAILED,
        DataSourceStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataSourceStatus = DataSourceStatus'
  { fromDataSourceStatus ::
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

pattern DataSourceStatus_ACTIVE :: DataSourceStatus
pattern DataSourceStatus_ACTIVE = DataSourceStatus' "ACTIVE"

pattern DataSourceStatus_CREATING :: DataSourceStatus
pattern DataSourceStatus_CREATING = DataSourceStatus' "CREATING"

pattern DataSourceStatus_DELETING :: DataSourceStatus
pattern DataSourceStatus_DELETING = DataSourceStatus' "DELETING"

pattern DataSourceStatus_FAILED :: DataSourceStatus
pattern DataSourceStatus_FAILED = DataSourceStatus' "FAILED"

pattern DataSourceStatus_UPDATING :: DataSourceStatus
pattern DataSourceStatus_UPDATING = DataSourceStatus' "UPDATING"

{-# COMPLETE
  DataSourceStatus_ACTIVE,
  DataSourceStatus_CREATING,
  DataSourceStatus_DELETING,
  DataSourceStatus_FAILED,
  DataSourceStatus_UPDATING,
  DataSourceStatus'
  #-}
