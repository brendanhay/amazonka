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
-- Module      : Amazonka.QuickSight.Types.SheetControlListType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetControlListType
  ( SheetControlListType
      ( ..,
        SheetControlListType_MULTI_SELECT,
        SheetControlListType_SINGLE_SELECT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SheetControlListType = SheetControlListType'
  { fromSheetControlListType ::
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

pattern SheetControlListType_MULTI_SELECT :: SheetControlListType
pattern SheetControlListType_MULTI_SELECT = SheetControlListType' "MULTI_SELECT"

pattern SheetControlListType_SINGLE_SELECT :: SheetControlListType
pattern SheetControlListType_SINGLE_SELECT = SheetControlListType' "SINGLE_SELECT"

{-# COMPLETE
  SheetControlListType_MULTI_SELECT,
  SheetControlListType_SINGLE_SELECT,
  SheetControlListType'
  #-}
