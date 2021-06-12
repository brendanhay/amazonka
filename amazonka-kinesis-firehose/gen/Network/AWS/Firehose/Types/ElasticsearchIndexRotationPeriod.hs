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
-- Module      : Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ElasticsearchIndexRotationPeriod
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

import qualified Network.AWS.Core as Core

newtype ElasticsearchIndexRotationPeriod = ElasticsearchIndexRotationPeriod'
  { fromElasticsearchIndexRotationPeriod ::
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
