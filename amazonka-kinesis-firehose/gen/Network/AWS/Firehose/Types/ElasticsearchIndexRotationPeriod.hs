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

import qualified Network.AWS.Prelude as Prelude

newtype ElasticsearchIndexRotationPeriod = ElasticsearchIndexRotationPeriod'
  { fromElasticsearchIndexRotationPeriod ::
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
