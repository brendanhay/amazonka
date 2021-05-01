{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.CompressionFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.CompressionFormat
  ( CompressionFormat
      ( ..,
        CompressionFormat_GZIP,
        CompressionFormat_Parquet,
        CompressionFormat_ZIP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | The compression format that AWS uses for the report.
newtype CompressionFormat = CompressionFormat'
  { fromCompressionFormat ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern CompressionFormat_GZIP :: CompressionFormat
pattern CompressionFormat_GZIP = CompressionFormat' "GZIP"

pattern CompressionFormat_Parquet :: CompressionFormat
pattern CompressionFormat_Parquet = CompressionFormat' "Parquet"

pattern CompressionFormat_ZIP :: CompressionFormat
pattern CompressionFormat_ZIP = CompressionFormat' "ZIP"

{-# COMPLETE
  CompressionFormat_GZIP,
  CompressionFormat_Parquet,
  CompressionFormat_ZIP,
  CompressionFormat'
  #-}
