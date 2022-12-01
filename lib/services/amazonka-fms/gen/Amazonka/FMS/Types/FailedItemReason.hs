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
-- Module      : Amazonka.FMS.Types.FailedItemReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.FailedItemReason
  ( FailedItemReason
      ( ..,
        FailedItemReason_NOT_VALID_ACCOUNT_ID,
        FailedItemReason_NOT_VALID_ARN,
        FailedItemReason_NOT_VALID_PARTITION,
        FailedItemReason_NOT_VALID_REGION,
        FailedItemReason_NOT_VALID_RESOURCE_TYPE,
        FailedItemReason_NOT_VALID_SERVICE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FailedItemReason = FailedItemReason'
  { fromFailedItemReason ::
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

pattern FailedItemReason_NOT_VALID_ACCOUNT_ID :: FailedItemReason
pattern FailedItemReason_NOT_VALID_ACCOUNT_ID = FailedItemReason' "NOT_VALID_ACCOUNT_ID"

pattern FailedItemReason_NOT_VALID_ARN :: FailedItemReason
pattern FailedItemReason_NOT_VALID_ARN = FailedItemReason' "NOT_VALID_ARN"

pattern FailedItemReason_NOT_VALID_PARTITION :: FailedItemReason
pattern FailedItemReason_NOT_VALID_PARTITION = FailedItemReason' "NOT_VALID_PARTITION"

pattern FailedItemReason_NOT_VALID_REGION :: FailedItemReason
pattern FailedItemReason_NOT_VALID_REGION = FailedItemReason' "NOT_VALID_REGION"

pattern FailedItemReason_NOT_VALID_RESOURCE_TYPE :: FailedItemReason
pattern FailedItemReason_NOT_VALID_RESOURCE_TYPE = FailedItemReason' "NOT_VALID_RESOURCE_TYPE"

pattern FailedItemReason_NOT_VALID_SERVICE :: FailedItemReason
pattern FailedItemReason_NOT_VALID_SERVICE = FailedItemReason' "NOT_VALID_SERVICE"

{-# COMPLETE
  FailedItemReason_NOT_VALID_ACCOUNT_ID,
  FailedItemReason_NOT_VALID_ARN,
  FailedItemReason_NOT_VALID_PARTITION,
  FailedItemReason_NOT_VALID_REGION,
  FailedItemReason_NOT_VALID_RESOURCE_TYPE,
  FailedItemReason_NOT_VALID_SERVICE,
  FailedItemReason'
  #-}
