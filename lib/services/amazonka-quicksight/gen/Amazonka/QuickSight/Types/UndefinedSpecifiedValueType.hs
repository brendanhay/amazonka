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
-- Module      : Amazonka.QuickSight.Types.UndefinedSpecifiedValueType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.UndefinedSpecifiedValueType
  ( UndefinedSpecifiedValueType
      ( ..,
        UndefinedSpecifiedValueType_LEAST,
        UndefinedSpecifiedValueType_MOST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UndefinedSpecifiedValueType = UndefinedSpecifiedValueType'
  { fromUndefinedSpecifiedValueType ::
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

pattern UndefinedSpecifiedValueType_LEAST :: UndefinedSpecifiedValueType
pattern UndefinedSpecifiedValueType_LEAST = UndefinedSpecifiedValueType' "LEAST"

pattern UndefinedSpecifiedValueType_MOST :: UndefinedSpecifiedValueType
pattern UndefinedSpecifiedValueType_MOST = UndefinedSpecifiedValueType' "MOST"

{-# COMPLETE
  UndefinedSpecifiedValueType_LEAST,
  UndefinedSpecifiedValueType_MOST,
  UndefinedSpecifiedValueType'
  #-}
