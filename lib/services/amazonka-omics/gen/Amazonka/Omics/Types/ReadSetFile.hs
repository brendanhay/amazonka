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
-- Module      : Amazonka.Omics.Types.ReadSetFile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReadSetFile
  ( ReadSetFile
      ( ..,
        ReadSetFile_INDEX,
        ReadSetFile_SOURCE1,
        ReadSetFile_SOURCE2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReadSetFile = ReadSetFile'
  { fromReadSetFile ::
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

pattern ReadSetFile_INDEX :: ReadSetFile
pattern ReadSetFile_INDEX = ReadSetFile' "INDEX"

pattern ReadSetFile_SOURCE1 :: ReadSetFile
pattern ReadSetFile_SOURCE1 = ReadSetFile' "SOURCE1"

pattern ReadSetFile_SOURCE2 :: ReadSetFile
pattern ReadSetFile_SOURCE2 = ReadSetFile' "SOURCE2"

{-# COMPLETE
  ReadSetFile_INDEX,
  ReadSetFile_SOURCE1,
  ReadSetFile_SOURCE2,
  ReadSetFile'
  #-}
