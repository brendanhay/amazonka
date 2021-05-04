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

import qualified Network.AWS.Prelude as Prelude

newtype ParallelDataFormat = ParallelDataFormat'
  { fromParallelDataFormat ::
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
