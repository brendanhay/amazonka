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
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportInterval
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportInterval
  ( BusinessReportInterval
      ( ..,
        BusinessReportInterval_ONE_DAY,
        BusinessReportInterval_ONE_WEEK,
        BusinessReportInterval_THIRTY_DAYS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BusinessReportInterval = BusinessReportInterval'
  { fromBusinessReportInterval ::
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

pattern BusinessReportInterval_ONE_DAY :: BusinessReportInterval
pattern BusinessReportInterval_ONE_DAY = BusinessReportInterval' "ONE_DAY"

pattern BusinessReportInterval_ONE_WEEK :: BusinessReportInterval
pattern BusinessReportInterval_ONE_WEEK = BusinessReportInterval' "ONE_WEEK"

pattern BusinessReportInterval_THIRTY_DAYS :: BusinessReportInterval
pattern BusinessReportInterval_THIRTY_DAYS = BusinessReportInterval' "THIRTY_DAYS"

{-# COMPLETE
  BusinessReportInterval_ONE_DAY,
  BusinessReportInterval_ONE_WEEK,
  BusinessReportInterval_THIRTY_DAYS,
  BusinessReportInterval'
  #-}
