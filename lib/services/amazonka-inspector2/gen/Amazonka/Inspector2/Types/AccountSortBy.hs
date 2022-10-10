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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype AccountSortBy = AccountSortBy'
  { fromAccountSortBy ::
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
