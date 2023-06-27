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
-- Module      : Amazonka.QuickSight.Types.DatasetParameterValueType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DatasetParameterValueType
  ( DatasetParameterValueType
      ( ..,
        DatasetParameterValueType_MULTI_VALUED,
        DatasetParameterValueType_SINGLE_VALUED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatasetParameterValueType = DatasetParameterValueType'
  { fromDatasetParameterValueType ::
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

pattern DatasetParameterValueType_MULTI_VALUED :: DatasetParameterValueType
pattern DatasetParameterValueType_MULTI_VALUED = DatasetParameterValueType' "MULTI_VALUED"

pattern DatasetParameterValueType_SINGLE_VALUED :: DatasetParameterValueType
pattern DatasetParameterValueType_SINGLE_VALUED = DatasetParameterValueType' "SINGLE_VALUED"

{-# COMPLETE
  DatasetParameterValueType_MULTI_VALUED,
  DatasetParameterValueType_SINGLE_VALUED,
  DatasetParameterValueType'
  #-}
