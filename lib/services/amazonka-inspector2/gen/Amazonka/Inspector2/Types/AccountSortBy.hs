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
-- Module      : Amazonka.Inspector2.Types.AccountSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AccountSortBy
  ( AccountSortBy
      ( ..,
        AccountSortBy_ALL,
        AccountSortBy_CRITICAL,
        AccountSortBy_HIGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccountSortBy = AccountSortBy'
  { fromAccountSortBy ::
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

pattern AccountSortBy_ALL :: AccountSortBy
pattern AccountSortBy_ALL = AccountSortBy' "ALL"

pattern AccountSortBy_CRITICAL :: AccountSortBy
pattern AccountSortBy_CRITICAL = AccountSortBy' "CRITICAL"

pattern AccountSortBy_HIGH :: AccountSortBy
pattern AccountSortBy_HIGH = AccountSortBy' "HIGH"

{-# COMPLETE
  AccountSortBy_ALL,
  AccountSortBy_CRITICAL,
  AccountSortBy_HIGH,
  AccountSortBy'
  #-}
