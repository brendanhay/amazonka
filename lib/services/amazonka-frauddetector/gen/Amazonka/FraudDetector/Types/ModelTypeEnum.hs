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
-- Module      : Amazonka.FraudDetector.Types.ModelTypeEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelTypeEnum
  ( ModelTypeEnum
      ( ..,
        ModelTypeEnum_ACCOUNT_TAKEOVER_INSIGHTS,
        ModelTypeEnum_ONLINE_FRAUD_INSIGHTS,
        ModelTypeEnum_TRANSACTION_FRAUD_INSIGHTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ModelTypeEnum = ModelTypeEnum'
  { fromModelTypeEnum ::
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

pattern ModelTypeEnum_ACCOUNT_TAKEOVER_INSIGHTS :: ModelTypeEnum
pattern ModelTypeEnum_ACCOUNT_TAKEOVER_INSIGHTS = ModelTypeEnum' "ACCOUNT_TAKEOVER_INSIGHTS"

pattern ModelTypeEnum_ONLINE_FRAUD_INSIGHTS :: ModelTypeEnum
pattern ModelTypeEnum_ONLINE_FRAUD_INSIGHTS = ModelTypeEnum' "ONLINE_FRAUD_INSIGHTS"

pattern ModelTypeEnum_TRANSACTION_FRAUD_INSIGHTS :: ModelTypeEnum
pattern ModelTypeEnum_TRANSACTION_FRAUD_INSIGHTS = ModelTypeEnum' "TRANSACTION_FRAUD_INSIGHTS"

{-# COMPLETE
  ModelTypeEnum_ACCOUNT_TAKEOVER_INSIGHTS,
  ModelTypeEnum_ONLINE_FRAUD_INSIGHTS,
  ModelTypeEnum_TRANSACTION_FRAUD_INSIGHTS,
  ModelTypeEnum'
  #-}
