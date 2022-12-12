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
-- Module      : Amazonka.SageMaker.Types.SortInferenceExperimentsBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SortInferenceExperimentsBy
  ( SortInferenceExperimentsBy
      ( ..,
        SortInferenceExperimentsBy_CreationTime,
        SortInferenceExperimentsBy_Name,
        SortInferenceExperimentsBy_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SortInferenceExperimentsBy = SortInferenceExperimentsBy'
  { fromSortInferenceExperimentsBy ::
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

pattern SortInferenceExperimentsBy_CreationTime :: SortInferenceExperimentsBy
pattern SortInferenceExperimentsBy_CreationTime = SortInferenceExperimentsBy' "CreationTime"

pattern SortInferenceExperimentsBy_Name :: SortInferenceExperimentsBy
pattern SortInferenceExperimentsBy_Name = SortInferenceExperimentsBy' "Name"

pattern SortInferenceExperimentsBy_Status :: SortInferenceExperimentsBy
pattern SortInferenceExperimentsBy_Status = SortInferenceExperimentsBy' "Status"

{-# COMPLETE
  SortInferenceExperimentsBy_CreationTime,
  SortInferenceExperimentsBy_Name,
  SortInferenceExperimentsBy_Status,
  SortInferenceExperimentsBy'
  #-}
