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
-- Module      : Amazonka.RDSData.Types.DecimalReturnType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Types.DecimalReturnType
  ( DecimalReturnType
      ( ..,
        DecimalReturnType_DOUBLE_OR_LONG,
        DecimalReturnType_STRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DecimalReturnType = DecimalReturnType'
  { fromDecimalReturnType ::
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

pattern DecimalReturnType_DOUBLE_OR_LONG :: DecimalReturnType
pattern DecimalReturnType_DOUBLE_OR_LONG = DecimalReturnType' "DOUBLE_OR_LONG"

pattern DecimalReturnType_STRING :: DecimalReturnType
pattern DecimalReturnType_STRING = DecimalReturnType' "STRING"

{-# COMPLETE
  DecimalReturnType_DOUBLE_OR_LONG,
  DecimalReturnType_STRING,
  DecimalReturnType'
  #-}
