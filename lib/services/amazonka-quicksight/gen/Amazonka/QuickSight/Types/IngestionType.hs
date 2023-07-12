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
-- Module      : Amazonka.QuickSight.Types.IngestionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IngestionType
  ( IngestionType
      ( ..,
        IngestionType_FULL_REFRESH,
        IngestionType_INCREMENTAL_REFRESH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This defines the type of ingestion user wants to trigger. This is part
-- of create ingestion request.
newtype IngestionType = IngestionType'
  { fromIngestionType ::
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

pattern IngestionType_FULL_REFRESH :: IngestionType
pattern IngestionType_FULL_REFRESH = IngestionType' "FULL_REFRESH"

pattern IngestionType_INCREMENTAL_REFRESH :: IngestionType
pattern IngestionType_INCREMENTAL_REFRESH = IngestionType' "INCREMENTAL_REFRESH"

{-# COMPLETE
  IngestionType_FULL_REFRESH,
  IngestionType_INCREMENTAL_REFRESH,
  IngestionType'
  #-}
