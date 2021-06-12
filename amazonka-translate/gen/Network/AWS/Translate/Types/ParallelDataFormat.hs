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
-- Module      : Network.AWS.Translate.Types.ParallelDataFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.ParallelDataFormat
  ( ParallelDataFormat
      ( ..,
        ParallelDataFormat_CSV,
        ParallelDataFormat_TMX,
        ParallelDataFormat_TSV
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ParallelDataFormat = ParallelDataFormat'
  { fromParallelDataFormat ::
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
