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
-- Module      : Amazonka.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
  ( ReturnValuesOnConditionCheckFailure
      ( ..,
        ReturnValuesOnConditionCheckFailure_ALL_OLD,
        ReturnValuesOnConditionCheckFailure_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype ReturnValuesOnConditionCheckFailure = ReturnValuesOnConditionCheckFailure'
  { fromReturnValuesOnConditionCheckFailure ::
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

pattern ReturnValuesOnConditionCheckFailure_ALL_OLD :: ReturnValuesOnConditionCheckFailure
pattern ReturnValuesOnConditionCheckFailure_ALL_OLD = ReturnValuesOnConditionCheckFailure' "ALL_OLD"

pattern ReturnValuesOnConditionCheckFailure_NONE :: ReturnValuesOnConditionCheckFailure
pattern ReturnValuesOnConditionCheckFailure_NONE = ReturnValuesOnConditionCheckFailure' "NONE"

{-# COMPLETE
  ReturnValuesOnConditionCheckFailure_ALL_OLD,
  ReturnValuesOnConditionCheckFailure_NONE,
  ReturnValuesOnConditionCheckFailure'
  #-}
