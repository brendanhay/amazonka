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
-- Module      : Amazonka.QuickSight.Types.DataLabelContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataLabelContent
  ( DataLabelContent
      ( ..,
        DataLabelContent_PERCENT,
        DataLabelContent_VALUE,
        DataLabelContent_VALUE_AND_PERCENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataLabelContent = DataLabelContent'
  { fromDataLabelContent ::
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

pattern DataLabelContent_PERCENT :: DataLabelContent
pattern DataLabelContent_PERCENT = DataLabelContent' "PERCENT"

pattern DataLabelContent_VALUE :: DataLabelContent
pattern DataLabelContent_VALUE = DataLabelContent' "VALUE"

pattern DataLabelContent_VALUE_AND_PERCENT :: DataLabelContent
pattern DataLabelContent_VALUE_AND_PERCENT = DataLabelContent' "VALUE_AND_PERCENT"

{-# COMPLETE
  DataLabelContent_PERCENT,
  DataLabelContent_VALUE,
  DataLabelContent_VALUE_AND_PERCENT,
  DataLabelContent'
  #-}
