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
-- Module      : Amazonka.IoTSiteWise.Types.ListTimeSeriesType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ListTimeSeriesType
  ( ListTimeSeriesType
      ( ..,
        ListTimeSeriesType_ASSOCIATED,
        ListTimeSeriesType_DISASSOCIATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ListTimeSeriesType = ListTimeSeriesType'
  { fromListTimeSeriesType ::
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

pattern ListTimeSeriesType_ASSOCIATED :: ListTimeSeriesType
pattern ListTimeSeriesType_ASSOCIATED = ListTimeSeriesType' "ASSOCIATED"

pattern ListTimeSeriesType_DISASSOCIATED :: ListTimeSeriesType
pattern ListTimeSeriesType_DISASSOCIATED = ListTimeSeriesType' "DISASSOCIATED"

{-# COMPLETE
  ListTimeSeriesType_ASSOCIATED,
  ListTimeSeriesType_DISASSOCIATED,
  ListTimeSeriesType'
  #-}
