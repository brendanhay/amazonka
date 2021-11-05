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
-- Module      : Network.AWS.FinSpaceData.Types.FormatType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FinSpaceData.Types.FormatType
  ( FormatType
      ( ..,
        FormatType_CSV,
        FormatType_JSON,
        FormatType_PARQUET,
        FormatType_XML
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FormatType = FormatType'
  { fromFormatType ::
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

pattern FormatType_CSV :: FormatType
pattern FormatType_CSV = FormatType' "CSV"

pattern FormatType_JSON :: FormatType
pattern FormatType_JSON = FormatType' "JSON"

pattern FormatType_PARQUET :: FormatType
pattern FormatType_PARQUET = FormatType' "PARQUET"

pattern FormatType_XML :: FormatType
pattern FormatType_XML = FormatType' "XML"

{-# COMPLETE
  FormatType_CSV,
  FormatType_JSON,
  FormatType_PARQUET,
  FormatType_XML,
  FormatType'
  #-}
