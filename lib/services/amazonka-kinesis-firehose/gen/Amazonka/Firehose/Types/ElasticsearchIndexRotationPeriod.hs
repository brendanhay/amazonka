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
-- Module      : Amazonka.Firehose.Types.ElasticsearchIndexRotationPeriod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.ElasticsearchIndexRotationPeriod
  ( ElasticsearchIndexRotationPeriod
      ( ..,
        ElasticsearchIndexRotationPeriod_NoRotation,
        ElasticsearchIndexRotationPeriod_OneDay,
        ElasticsearchIndexRotationPeriod_OneHour,
        ElasticsearchIndexRotationPeriod_OneMonth,
        ElasticsearchIndexRotationPeriod_OneWeek
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ElasticsearchIndexRotationPeriod = ElasticsearchIndexRotationPeriod'
  { fromElasticsearchIndexRotationPeriod ::
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

pattern ElasticsearchIndexRotationPeriod_NoRotation :: ElasticsearchIndexRotationPeriod
pattern ElasticsearchIndexRotationPeriod_NoRotation = ElasticsearchIndexRotationPeriod' "NoRotation"

pattern ElasticsearchIndexRotationPeriod_OneDay :: ElasticsearchIndexRotationPeriod
pattern ElasticsearchIndexRotationPeriod_OneDay = ElasticsearchIndexRotationPeriod' "OneDay"

pattern ElasticsearchIndexRotationPeriod_OneHour :: ElasticsearchIndexRotationPeriod
pattern ElasticsearchIndexRotationPeriod_OneHour = ElasticsearchIndexRotationPeriod' "OneHour"

pattern ElasticsearchIndexRotationPeriod_OneMonth :: ElasticsearchIndexRotationPeriod
pattern ElasticsearchIndexRotationPeriod_OneMonth = ElasticsearchIndexRotationPeriod' "OneMonth"

pattern ElasticsearchIndexRotationPeriod_OneWeek :: ElasticsearchIndexRotationPeriod
pattern ElasticsearchIndexRotationPeriod_OneWeek = ElasticsearchIndexRotationPeriod' "OneWeek"

{-# COMPLETE
  ElasticsearchIndexRotationPeriod_NoRotation,
  ElasticsearchIndexRotationPeriod_OneDay,
  ElasticsearchIndexRotationPeriod_OneHour,
  ElasticsearchIndexRotationPeriod_OneMonth,
  ElasticsearchIndexRotationPeriod_OneWeek,
  ElasticsearchIndexRotationPeriod'
  #-}
