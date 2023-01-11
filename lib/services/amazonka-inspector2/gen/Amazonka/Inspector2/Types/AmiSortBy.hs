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
-- Module      : Amazonka.Inspector2.Types.AmiSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AmiSortBy
  ( AmiSortBy
      ( ..,
        AmiSortBy_AFFECTED_INSTANCES,
        AmiSortBy_ALL,
        AmiSortBy_CRITICAL,
        AmiSortBy_HIGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AmiSortBy = AmiSortBy'
  { fromAmiSortBy ::
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

pattern AmiSortBy_AFFECTED_INSTANCES :: AmiSortBy
pattern AmiSortBy_AFFECTED_INSTANCES = AmiSortBy' "AFFECTED_INSTANCES"

pattern AmiSortBy_ALL :: AmiSortBy
pattern AmiSortBy_ALL = AmiSortBy' "ALL"

pattern AmiSortBy_CRITICAL :: AmiSortBy
pattern AmiSortBy_CRITICAL = AmiSortBy' "CRITICAL"

pattern AmiSortBy_HIGH :: AmiSortBy
pattern AmiSortBy_HIGH = AmiSortBy' "HIGH"

{-# COMPLETE
  AmiSortBy_AFFECTED_INSTANCES,
  AmiSortBy_ALL,
  AmiSortBy_CRITICAL,
  AmiSortBy_HIGH,
  AmiSortBy'
  #-}
