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
-- Module      : Amazonka.AlexaBusiness.Types.BusinessReportInterval
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.BusinessReportInterval
  ( BusinessReportInterval
      ( ..,
        BusinessReportInterval_ONE_DAY,
        BusinessReportInterval_ONE_WEEK,
        BusinessReportInterval_THIRTY_DAYS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BusinessReportInterval = BusinessReportInterval'
  { fromBusinessReportInterval ::
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
