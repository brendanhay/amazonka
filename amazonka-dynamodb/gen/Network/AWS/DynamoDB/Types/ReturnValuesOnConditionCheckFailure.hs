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
-- Module      : Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ReturnValuesOnConditionCheckFailure
  ( ReturnValuesOnConditionCheckFailure
      ( ..,
        ReturnValuesOnConditionCheckFailure_ALL_OLD,
        ReturnValuesOnConditionCheckFailure_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ReturnValuesOnConditionCheckFailure = ReturnValuesOnConditionCheckFailure'
  { fromReturnValuesOnConditionCheckFailure ::
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

pattern ReturnValuesOnConditionCheckFailure_ALL_OLD :: ReturnValuesOnConditionCheckFailure
pattern ReturnValuesOnConditionCheckFailure_ALL_OLD = ReturnValuesOnConditionCheckFailure' "ALL_OLD"

pattern ReturnValuesOnConditionCheckFailure_NONE :: ReturnValuesOnConditionCheckFailure
pattern ReturnValuesOnConditionCheckFailure_NONE = ReturnValuesOnConditionCheckFailure' "NONE"

{-# COMPLETE
  ReturnValuesOnConditionCheckFailure_ALL_OLD,
  ReturnValuesOnConditionCheckFailure_NONE,
  ReturnValuesOnConditionCheckFailure'
  #-}
