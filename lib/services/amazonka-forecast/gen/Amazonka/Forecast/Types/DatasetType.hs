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
-- Module      : Amazonka.Forecast.Types.DatasetType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.DatasetType
  ( DatasetType
      ( ..,
        DatasetType_ITEM_METADATA,
        DatasetType_RELATED_TIME_SERIES,
        DatasetType_TARGET_TIME_SERIES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatasetType = DatasetType'
  { fromDatasetType ::
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

pattern DatasetType_ITEM_METADATA :: DatasetType
pattern DatasetType_ITEM_METADATA = DatasetType' "ITEM_METADATA"

pattern DatasetType_RELATED_TIME_SERIES :: DatasetType
pattern DatasetType_RELATED_TIME_SERIES = DatasetType' "RELATED_TIME_SERIES"

pattern DatasetType_TARGET_TIME_SERIES :: DatasetType
pattern DatasetType_TARGET_TIME_SERIES = DatasetType' "TARGET_TIME_SERIES"

{-# COMPLETE
  DatasetType_ITEM_METADATA,
  DatasetType_RELATED_TIME_SERIES,
  DatasetType_TARGET_TIME_SERIES,
  DatasetType'
  #-}
