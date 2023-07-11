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
-- Module      : Amazonka.KinesisVideo.Types.UpdateDataRetentionOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.UpdateDataRetentionOperation
  ( UpdateDataRetentionOperation
      ( ..,
        UpdateDataRetentionOperation_DECREASE_DATA_RETENTION,
        UpdateDataRetentionOperation_INCREASE_DATA_RETENTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UpdateDataRetentionOperation = UpdateDataRetentionOperation'
  { fromUpdateDataRetentionOperation ::
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

pattern UpdateDataRetentionOperation_DECREASE_DATA_RETENTION :: UpdateDataRetentionOperation
pattern UpdateDataRetentionOperation_DECREASE_DATA_RETENTION = UpdateDataRetentionOperation' "DECREASE_DATA_RETENTION"

pattern UpdateDataRetentionOperation_INCREASE_DATA_RETENTION :: UpdateDataRetentionOperation
pattern UpdateDataRetentionOperation_INCREASE_DATA_RETENTION = UpdateDataRetentionOperation' "INCREASE_DATA_RETENTION"

{-# COMPLETE
  UpdateDataRetentionOperation_DECREASE_DATA_RETENTION,
  UpdateDataRetentionOperation_INCREASE_DATA_RETENTION,
  UpdateDataRetentionOperation'
  #-}
