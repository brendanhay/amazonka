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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.FilterCondition
  ( FilterCondition
      ( ..,
        FilterCondition_BEGINS_WITH,
        FilterCondition_BETWEEN,
        FilterCondition_EQ,
        FilterCondition_IN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FilterCondition = FilterCondition'
  { fromFilterCondition ::
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

pattern FilterCondition_BEGINS_WITH :: FilterCondition
pattern FilterCondition_BEGINS_WITH = FilterCondition' "BEGINS_WITH"

pattern FilterCondition_BETWEEN :: FilterCondition
pattern FilterCondition_BETWEEN = FilterCondition' "BETWEEN"

pattern FilterCondition_EQ :: FilterCondition
pattern FilterCondition_EQ = FilterCondition' "EQ"

pattern FilterCondition_IN :: FilterCondition
pattern FilterCondition_IN = FilterCondition' "IN"

{-# COMPLETE
  FilterCondition_BEGINS_WITH,
  FilterCondition_BETWEEN,
  FilterCondition_EQ,
  FilterCondition_IN,
  FilterCondition'
  #-}
