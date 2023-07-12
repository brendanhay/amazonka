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
-- Module      : Amazonka.SageMaker.Types.ModelCardExportJobSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardExportJobSortBy
  ( ModelCardExportJobSortBy
      ( ..,
        ModelCardExportJobSortBy_CreationTime,
        ModelCardExportJobSortBy_Name,
        ModelCardExportJobSortBy_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Attribute by which to sort returned export jobs.
newtype ModelCardExportJobSortBy = ModelCardExportJobSortBy'
  { fromModelCardExportJobSortBy ::
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

pattern ModelCardExportJobSortBy_CreationTime :: ModelCardExportJobSortBy
pattern ModelCardExportJobSortBy_CreationTime = ModelCardExportJobSortBy' "CreationTime"

pattern ModelCardExportJobSortBy_Name :: ModelCardExportJobSortBy
pattern ModelCardExportJobSortBy_Name = ModelCardExportJobSortBy' "Name"

pattern ModelCardExportJobSortBy_Status :: ModelCardExportJobSortBy
pattern ModelCardExportJobSortBy_Status = ModelCardExportJobSortBy' "Status"

{-# COMPLETE
  ModelCardExportJobSortBy_CreationTime,
  ModelCardExportJobSortBy_Name,
  ModelCardExportJobSortBy_Status,
  ModelCardExportJobSortBy'
  #-}
