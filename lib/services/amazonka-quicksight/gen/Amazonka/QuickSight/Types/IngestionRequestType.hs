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
-- Module      : Amazonka.QuickSight.Types.IngestionRequestType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.IngestionRequestType
  ( IngestionRequestType
      ( ..,
        IngestionRequestType_EDIT,
        IngestionRequestType_FULL_REFRESH,
        IngestionRequestType_INCREMENTAL_REFRESH,
        IngestionRequestType_INITIAL_INGESTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | This defines the type of ingestion request. This is returned as part of
-- create ingestion response.
newtype IngestionRequestType = IngestionRequestType'
  { fromIngestionRequestType ::
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

pattern IngestionRequestType_EDIT :: IngestionRequestType
pattern IngestionRequestType_EDIT = IngestionRequestType' "EDIT"

pattern IngestionRequestType_FULL_REFRESH :: IngestionRequestType
pattern IngestionRequestType_FULL_REFRESH = IngestionRequestType' "FULL_REFRESH"

pattern IngestionRequestType_INCREMENTAL_REFRESH :: IngestionRequestType
pattern IngestionRequestType_INCREMENTAL_REFRESH = IngestionRequestType' "INCREMENTAL_REFRESH"

pattern IngestionRequestType_INITIAL_INGESTION :: IngestionRequestType
pattern IngestionRequestType_INITIAL_INGESTION = IngestionRequestType' "INITIAL_INGESTION"

{-# COMPLETE
  IngestionRequestType_EDIT,
  IngestionRequestType_FULL_REFRESH,
  IngestionRequestType_INCREMENTAL_REFRESH,
  IngestionRequestType_INITIAL_INGESTION,
  IngestionRequestType'
  #-}
