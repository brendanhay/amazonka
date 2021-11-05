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
-- Module      : Amazonka.SageMaker.Types.RedshiftResultFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RedshiftResultFormat
  ( RedshiftResultFormat
      ( ..,
        RedshiftResultFormat_CSV,
        RedshiftResultFormat_PARQUET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The data storage format for Redshift query results.
newtype RedshiftResultFormat = RedshiftResultFormat'
  { fromRedshiftResultFormat ::
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

pattern RedshiftResultFormat_CSV :: RedshiftResultFormat
pattern RedshiftResultFormat_CSV = RedshiftResultFormat' "CSV"

pattern RedshiftResultFormat_PARQUET :: RedshiftResultFormat
pattern RedshiftResultFormat_PARQUET = RedshiftResultFormat' "PARQUET"

{-# COMPLETE
  RedshiftResultFormat_CSV,
  RedshiftResultFormat_PARQUET,
  RedshiftResultFormat'
  #-}
