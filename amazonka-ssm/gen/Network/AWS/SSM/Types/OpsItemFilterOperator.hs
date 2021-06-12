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
-- Module      : Network.AWS.SSM.Types.OpsItemFilterOperator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemFilterOperator
  ( OpsItemFilterOperator
      ( ..,
        OpsItemFilterOperator_Contains,
        OpsItemFilterOperator_Equal,
        OpsItemFilterOperator_GreaterThan,
        OpsItemFilterOperator_LessThan
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OpsItemFilterOperator = OpsItemFilterOperator'
  { fromOpsItemFilterOperator ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
