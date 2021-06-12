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
-- Module      : Network.AWS.SageMaker.Types.RedshiftResultFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.RedshiftResultFormat
  ( RedshiftResultFormat
      ( ..,
        RedshiftResultFormat_CSV,
        RedshiftResultFormat_PARQUET
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The data storage format for Redshift query results.
newtype RedshiftResultFormat = RedshiftResultFormat'
  { fromRedshiftResultFormat ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
