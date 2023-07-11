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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DecimalReturnType = DecimalReturnType'
  { fromDecimalReturnType ::
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

pattern DecimalReturnType_DOUBLE_OR_LONG :: DecimalReturnType
pattern DecimalReturnType_DOUBLE_OR_LONG = DecimalReturnType' "DOUBLE_OR_LONG"

pattern DecimalReturnType_STRING :: DecimalReturnType
pattern DecimalReturnType_STRING = DecimalReturnType' "STRING"

{-# COMPLETE
  DecimalReturnType_DOUBLE_OR_LONG,
  DecimalReturnType_STRING,
  DecimalReturnType'
  #-}
