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
-- Module      : Amazonka.Route53AutoNaming.Types.FilterCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.FilterCondition
  ( FilterCondition
      ( ..,
        FilterCondition_BETWEEN,
        FilterCondition_EQ,
        FilterCondition_IN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FilterCondition = FilterCondition'
  { fromFilterCondition ::
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

pattern FilterCondition_BETWEEN :: FilterCondition
pattern FilterCondition_BETWEEN = FilterCondition' "BETWEEN"

pattern FilterCondition_EQ :: FilterCondition
pattern FilterCondition_EQ = FilterCondition' "EQ"

pattern FilterCondition_IN :: FilterCondition
pattern FilterCondition_IN = FilterCondition' "IN"

{-# COMPLETE
  FilterCondition_BETWEEN,
  FilterCondition_EQ,
  FilterCondition_IN,
  FilterCondition'
  #-}
