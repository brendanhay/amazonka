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
-- Module      : Amazonka.Glue.Types.FieldName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FieldName
  ( FieldName
      ( ..,
        FieldName_CRAWL_ID,
        FieldName_DPU_HOUR,
        FieldName_END_TIME,
        FieldName_START_TIME,
        FieldName_STATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FieldName = FieldName'
  { fromFieldName ::
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

pattern FieldName_CRAWL_ID :: FieldName
pattern FieldName_CRAWL_ID = FieldName' "CRAWL_ID"

pattern FieldName_DPU_HOUR :: FieldName
pattern FieldName_DPU_HOUR = FieldName' "DPU_HOUR"

pattern FieldName_END_TIME :: FieldName
pattern FieldName_END_TIME = FieldName' "END_TIME"

pattern FieldName_START_TIME :: FieldName
pattern FieldName_START_TIME = FieldName' "START_TIME"

pattern FieldName_STATE :: FieldName
pattern FieldName_STATE = FieldName' "STATE"

{-# COMPLETE
  FieldName_CRAWL_ID,
  FieldName_DPU_HOUR,
  FieldName_END_TIME,
  FieldName_START_TIME,
  FieldName_STATE,
  FieldName'
  #-}
