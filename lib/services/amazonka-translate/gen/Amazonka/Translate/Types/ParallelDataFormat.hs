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
-- Module      : Amazonka.Translate.Types.ParallelDataFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.ParallelDataFormat
  ( ParallelDataFormat
      ( ..,
        ParallelDataFormat_CSV,
        ParallelDataFormat_TMX,
        ParallelDataFormat_TSV
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ParallelDataFormat = ParallelDataFormat'
  { fromParallelDataFormat ::
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

pattern ParallelDataFormat_CSV :: ParallelDataFormat
pattern ParallelDataFormat_CSV = ParallelDataFormat' "CSV"

pattern ParallelDataFormat_TMX :: ParallelDataFormat
pattern ParallelDataFormat_TMX = ParallelDataFormat' "TMX"

pattern ParallelDataFormat_TSV :: ParallelDataFormat
pattern ParallelDataFormat_TSV = ParallelDataFormat' "TSV"

{-# COMPLETE
  ParallelDataFormat_CSV,
  ParallelDataFormat_TMX,
  ParallelDataFormat_TSV,
  ParallelDataFormat'
  #-}
