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
-- Module      : Amazonka.SSM.Types.OpsItemFilterOperator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsItemFilterOperator
  ( OpsItemFilterOperator
      ( ..,
        OpsItemFilterOperator_Contains,
        OpsItemFilterOperator_Equal,
        OpsItemFilterOperator_GreaterThan,
        OpsItemFilterOperator_LessThan
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype OpsItemFilterOperator = OpsItemFilterOperator'
  { fromOpsItemFilterOperator ::
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

pattern OpsItemFilterOperator_Contains :: OpsItemFilterOperator
pattern OpsItemFilterOperator_Contains = OpsItemFilterOperator' "Contains"

pattern OpsItemFilterOperator_Equal :: OpsItemFilterOperator
pattern OpsItemFilterOperator_Equal = OpsItemFilterOperator' "Equal"

pattern OpsItemFilterOperator_GreaterThan :: OpsItemFilterOperator
pattern OpsItemFilterOperator_GreaterThan = OpsItemFilterOperator' "GreaterThan"

pattern OpsItemFilterOperator_LessThan :: OpsItemFilterOperator
pattern OpsItemFilterOperator_LessThan = OpsItemFilterOperator' "LessThan"

{-# COMPLETE
  OpsItemFilterOperator_Contains,
  OpsItemFilterOperator_Equal,
  OpsItemFilterOperator_GreaterThan,
  OpsItemFilterOperator_LessThan,
  OpsItemFilterOperator'
  #-}
