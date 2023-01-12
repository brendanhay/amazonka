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
-- Module      : Amazonka.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.AmazonopensearchserviceIndexRotationPeriod
  ( AmazonopensearchserviceIndexRotationPeriod
      ( ..,
        AmazonopensearchserviceIndexRotationPeriod_NoRotation,
        AmazonopensearchserviceIndexRotationPeriod_OneDay,
        AmazonopensearchserviceIndexRotationPeriod_OneHour,
        AmazonopensearchserviceIndexRotationPeriod_OneMonth,
        AmazonopensearchserviceIndexRotationPeriod_OneWeek
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AmazonopensearchserviceIndexRotationPeriod = AmazonopensearchserviceIndexRotationPeriod'
  { fromAmazonopensearchserviceIndexRotationPeriod ::
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

pattern AmazonopensearchserviceIndexRotationPeriod_NoRotation :: AmazonopensearchserviceIndexRotationPeriod
pattern AmazonopensearchserviceIndexRotationPeriod_NoRotation = AmazonopensearchserviceIndexRotationPeriod' "NoRotation"

pattern AmazonopensearchserviceIndexRotationPeriod_OneDay :: AmazonopensearchserviceIndexRotationPeriod
pattern AmazonopensearchserviceIndexRotationPeriod_OneDay = AmazonopensearchserviceIndexRotationPeriod' "OneDay"

pattern AmazonopensearchserviceIndexRotationPeriod_OneHour :: AmazonopensearchserviceIndexRotationPeriod
pattern AmazonopensearchserviceIndexRotationPeriod_OneHour = AmazonopensearchserviceIndexRotationPeriod' "OneHour"

pattern AmazonopensearchserviceIndexRotationPeriod_OneMonth :: AmazonopensearchserviceIndexRotationPeriod
pattern AmazonopensearchserviceIndexRotationPeriod_OneMonth = AmazonopensearchserviceIndexRotationPeriod' "OneMonth"

pattern AmazonopensearchserviceIndexRotationPeriod_OneWeek :: AmazonopensearchserviceIndexRotationPeriod
pattern AmazonopensearchserviceIndexRotationPeriod_OneWeek = AmazonopensearchserviceIndexRotationPeriod' "OneWeek"

{-# COMPLETE
  AmazonopensearchserviceIndexRotationPeriod_NoRotation,
  AmazonopensearchserviceIndexRotationPeriod_OneDay,
  AmazonopensearchserviceIndexRotationPeriod_OneHour,
  AmazonopensearchserviceIndexRotationPeriod_OneMonth,
  AmazonopensearchserviceIndexRotationPeriod_OneWeek,
  AmazonopensearchserviceIndexRotationPeriod'
  #-}
