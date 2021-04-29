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

import qualified Network.AWS.Prelude as Prelude

newtype BusinessReportInterval = BusinessReportInterval'
  { fromBusinessReportInterval ::
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
