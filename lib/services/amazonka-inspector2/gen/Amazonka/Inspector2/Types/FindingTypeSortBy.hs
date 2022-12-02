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
-- Module      : Amazonka.Inspector2.Types.FindingTypeSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.FindingTypeSortBy
  ( FindingTypeSortBy
      ( ..,
        FindingTypeSortBy_ALL,
        FindingTypeSortBy_CRITICAL,
        FindingTypeSortBy_HIGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FindingTypeSortBy = FindingTypeSortBy'
  { fromFindingTypeSortBy ::
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

pattern FindingTypeSortBy_ALL :: FindingTypeSortBy
pattern FindingTypeSortBy_ALL = FindingTypeSortBy' "ALL"

pattern FindingTypeSortBy_CRITICAL :: FindingTypeSortBy
pattern FindingTypeSortBy_CRITICAL = FindingTypeSortBy' "CRITICAL"

pattern FindingTypeSortBy_HIGH :: FindingTypeSortBy
pattern FindingTypeSortBy_HIGH = FindingTypeSortBy' "HIGH"

{-# COMPLETE
  FindingTypeSortBy_ALL,
  FindingTypeSortBy_CRITICAL,
  FindingTypeSortBy_HIGH,
  FindingTypeSortBy'
  #-}
