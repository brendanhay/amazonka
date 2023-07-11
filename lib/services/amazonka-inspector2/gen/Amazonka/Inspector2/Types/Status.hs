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
-- Module      : Amazonka.Inspector2.Types.Status
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Status
  ( Status
      ( ..,
        Status_DISABLED,
        Status_DISABLING,
        Status_ENABLED,
        Status_ENABLING,
        Status_SUSPENDED,
        Status_SUSPENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Status = Status' {fromStatus :: Data.Text}
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

pattern Status_DISABLED :: Status
pattern Status_DISABLED = Status' "DISABLED"

pattern Status_DISABLING :: Status
pattern Status_DISABLING = Status' "DISABLING"

pattern Status_ENABLED :: Status
pattern Status_ENABLED = Status' "ENABLED"

pattern Status_ENABLING :: Status
pattern Status_ENABLING = Status' "ENABLING"

pattern Status_SUSPENDED :: Status
pattern Status_SUSPENDED = Status' "SUSPENDED"

pattern Status_SUSPENDING :: Status
pattern Status_SUSPENDING = Status' "SUSPENDING"

{-# COMPLETE
  Status_DISABLED,
  Status_DISABLING,
  Status_ENABLED,
  Status_ENABLING,
  Status_SUSPENDED,
  Status_SUSPENDING,
  Status'
  #-}
