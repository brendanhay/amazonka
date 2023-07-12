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
-- Module      : Amazonka.RDSData.Types.TypeHint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDSData.Types.TypeHint
  ( TypeHint
      ( ..,
        TypeHint_DATE,
        TypeHint_DECIMAL,
        TypeHint_JSON,
        TypeHint_TIME,
        TypeHint_TIMESTAMP,
        TypeHint_UUID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TypeHint = TypeHint'
  { fromTypeHint ::
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

pattern TypeHint_DATE :: TypeHint
pattern TypeHint_DATE = TypeHint' "DATE"

pattern TypeHint_DECIMAL :: TypeHint
pattern TypeHint_DECIMAL = TypeHint' "DECIMAL"

pattern TypeHint_JSON :: TypeHint
pattern TypeHint_JSON = TypeHint' "JSON"

pattern TypeHint_TIME :: TypeHint
pattern TypeHint_TIME = TypeHint' "TIME"

pattern TypeHint_TIMESTAMP :: TypeHint
pattern TypeHint_TIMESTAMP = TypeHint' "TIMESTAMP"

pattern TypeHint_UUID :: TypeHint
pattern TypeHint_UUID = TypeHint' "UUID"

{-# COMPLETE
  TypeHint_DATE,
  TypeHint_DECIMAL,
  TypeHint_JSON,
  TypeHint_TIME,
  TypeHint_TIMESTAMP,
  TypeHint_UUID,
  TypeHint'
  #-}
