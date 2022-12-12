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
-- Module      : Amazonka.QuickSight.Types.FilterNullOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterNullOption
  ( FilterNullOption
      ( ..,
        FilterNullOption_ALL_VALUES,
        FilterNullOption_NON_NULLS_ONLY,
        FilterNullOption_NULLS_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FilterNullOption = FilterNullOption'
  { fromFilterNullOption ::
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

pattern FilterNullOption_ALL_VALUES :: FilterNullOption
pattern FilterNullOption_ALL_VALUES = FilterNullOption' "ALL_VALUES"

pattern FilterNullOption_NON_NULLS_ONLY :: FilterNullOption
pattern FilterNullOption_NON_NULLS_ONLY = FilterNullOption' "NON_NULLS_ONLY"

pattern FilterNullOption_NULLS_ONLY :: FilterNullOption
pattern FilterNullOption_NULLS_ONLY = FilterNullOption' "NULLS_ONLY"

{-# COMPLETE
  FilterNullOption_ALL_VALUES,
  FilterNullOption_NON_NULLS_ONLY,
  FilterNullOption_NULLS_ONLY,
  FilterNullOption'
  #-}
