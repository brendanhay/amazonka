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
-- Module      : Amazonka.QuickSight.Types.TableTotalsScrollStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableTotalsScrollStatus
  ( TableTotalsScrollStatus
      ( ..,
        TableTotalsScrollStatus_PINNED,
        TableTotalsScrollStatus_SCROLLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TableTotalsScrollStatus = TableTotalsScrollStatus'
  { fromTableTotalsScrollStatus ::
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

pattern TableTotalsScrollStatus_PINNED :: TableTotalsScrollStatus
pattern TableTotalsScrollStatus_PINNED = TableTotalsScrollStatus' "PINNED"

pattern TableTotalsScrollStatus_SCROLLED :: TableTotalsScrollStatus
pattern TableTotalsScrollStatus_SCROLLED = TableTotalsScrollStatus' "SCROLLED"

{-# COMPLETE
  TableTotalsScrollStatus_PINNED,
  TableTotalsScrollStatus_SCROLLED,
  TableTotalsScrollStatus'
  #-}
