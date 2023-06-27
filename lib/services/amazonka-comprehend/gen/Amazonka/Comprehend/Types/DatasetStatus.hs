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
-- Module      : Amazonka.Comprehend.Types.DatasetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetStatus
  ( DatasetStatus
      ( ..,
        DatasetStatus_COMPLETED,
        DatasetStatus_CREATING,
        DatasetStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatasetStatus = DatasetStatus'
  { fromDatasetStatus ::
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

pattern DatasetStatus_COMPLETED :: DatasetStatus
pattern DatasetStatus_COMPLETED = DatasetStatus' "COMPLETED"

pattern DatasetStatus_CREATING :: DatasetStatus
pattern DatasetStatus_CREATING = DatasetStatus' "CREATING"

pattern DatasetStatus_FAILED :: DatasetStatus
pattern DatasetStatus_FAILED = DatasetStatus' "FAILED"

{-# COMPLETE
  DatasetStatus_COMPLETED,
  DatasetStatus_CREATING,
  DatasetStatus_FAILED,
  DatasetStatus'
  #-}
