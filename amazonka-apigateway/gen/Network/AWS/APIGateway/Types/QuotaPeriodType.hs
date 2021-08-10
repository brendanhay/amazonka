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
-- Module      : Network.AWS.APIGateway.Types.QuotaPeriodType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.QuotaPeriodType
  ( QuotaPeriodType
      ( ..,
        QuotaPeriodType_DAY,
        QuotaPeriodType_MONTH,
        QuotaPeriodType_WEEK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype QuotaPeriodType = QuotaPeriodType'
  { fromQuotaPeriodType ::
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

pattern QuotaPeriodType_DAY :: QuotaPeriodType
pattern QuotaPeriodType_DAY = QuotaPeriodType' "DAY"

pattern QuotaPeriodType_MONTH :: QuotaPeriodType
pattern QuotaPeriodType_MONTH = QuotaPeriodType' "MONTH"

pattern QuotaPeriodType_WEEK :: QuotaPeriodType
pattern QuotaPeriodType_WEEK = QuotaPeriodType' "WEEK"

{-# COMPLETE
  QuotaPeriodType_DAY,
  QuotaPeriodType_MONTH,
  QuotaPeriodType_WEEK,
  QuotaPeriodType'
  #-}
