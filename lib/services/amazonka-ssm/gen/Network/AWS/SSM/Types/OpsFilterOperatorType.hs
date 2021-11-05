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
-- Module      : Amazonka.SSM.Types.OpsFilterOperatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsFilterOperatorType
  ( OpsFilterOperatorType
      ( ..,
        OpsFilterOperatorType_BeginWith,
        OpsFilterOperatorType_Equal,
        OpsFilterOperatorType_Exists,
        OpsFilterOperatorType_GreaterThan,
        OpsFilterOperatorType_LessThan,
        OpsFilterOperatorType_NotEqual
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OpsFilterOperatorType = OpsFilterOperatorType'
  { fromOpsFilterOperatorType ::
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

pattern OpsFilterOperatorType_BeginWith :: OpsFilterOperatorType
pattern OpsFilterOperatorType_BeginWith = OpsFilterOperatorType' "BeginWith"

pattern OpsFilterOperatorType_Equal :: OpsFilterOperatorType
pattern OpsFilterOperatorType_Equal = OpsFilterOperatorType' "Equal"

pattern OpsFilterOperatorType_Exists :: OpsFilterOperatorType
pattern OpsFilterOperatorType_Exists = OpsFilterOperatorType' "Exists"

pattern OpsFilterOperatorType_GreaterThan :: OpsFilterOperatorType
pattern OpsFilterOperatorType_GreaterThan = OpsFilterOperatorType' "GreaterThan"

pattern OpsFilterOperatorType_LessThan :: OpsFilterOperatorType
pattern OpsFilterOperatorType_LessThan = OpsFilterOperatorType' "LessThan"

pattern OpsFilterOperatorType_NotEqual :: OpsFilterOperatorType
pattern OpsFilterOperatorType_NotEqual = OpsFilterOperatorType' "NotEqual"

{-# COMPLETE
  OpsFilterOperatorType_BeginWith,
  OpsFilterOperatorType_Equal,
  OpsFilterOperatorType_Exists,
  OpsFilterOperatorType_GreaterThan,
  OpsFilterOperatorType_LessThan,
  OpsFilterOperatorType_NotEqual,
  OpsFilterOperatorType'
  #-}
