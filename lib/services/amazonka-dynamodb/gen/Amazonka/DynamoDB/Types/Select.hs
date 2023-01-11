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
-- Module      : Amazonka.DynamoDB.Types.Select
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.Select
  ( Select
      ( ..,
        Select_ALL_ATTRIBUTES,
        Select_ALL_PROJECTED_ATTRIBUTES,
        Select_COUNT,
        Select_SPECIFIC_ATTRIBUTES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype Select = Select' {fromSelect :: Data.Text}
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

pattern Select_ALL_ATTRIBUTES :: Select
pattern Select_ALL_ATTRIBUTES = Select' "ALL_ATTRIBUTES"

pattern Select_ALL_PROJECTED_ATTRIBUTES :: Select
pattern Select_ALL_PROJECTED_ATTRIBUTES = Select' "ALL_PROJECTED_ATTRIBUTES"

pattern Select_COUNT :: Select
pattern Select_COUNT = Select' "COUNT"

pattern Select_SPECIFIC_ATTRIBUTES :: Select
pattern Select_SPECIFIC_ATTRIBUTES = Select' "SPECIFIC_ATTRIBUTES"

{-# COMPLETE
  Select_ALL_ATTRIBUTES,
  Select_ALL_PROJECTED_ATTRIBUTES,
  Select_COUNT,
  Select_SPECIFIC_ATTRIBUTES,
  Select'
  #-}
