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

import qualified Network.AWS.Prelude as Prelude

newtype QuotaPeriodType = QuotaPeriodType'
  { fromQuotaPeriodType ::
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
