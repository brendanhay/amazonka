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
-- Module      : Network.AWS.FraudDetector.Types.ModelTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FraudDetector.Types.ModelTypeEnum
  ( ModelTypeEnum
      ( ..,
        ModelTypeEnum_ONLINE_FRAUD_INSIGHTS,
        ModelTypeEnum_TRANSACTION_FRAUD_INSIGHTS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ModelTypeEnum = ModelTypeEnum'
  { fromModelTypeEnum ::
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

pattern ModelTypeEnum_ONLINE_FRAUD_INSIGHTS :: ModelTypeEnum
pattern ModelTypeEnum_ONLINE_FRAUD_INSIGHTS = ModelTypeEnum' "ONLINE_FRAUD_INSIGHTS"

pattern ModelTypeEnum_TRANSACTION_FRAUD_INSIGHTS :: ModelTypeEnum
pattern ModelTypeEnum_TRANSACTION_FRAUD_INSIGHTS = ModelTypeEnum' "TRANSACTION_FRAUD_INSIGHTS"

{-# COMPLETE
  ModelTypeEnum_ONLINE_FRAUD_INSIGHTS,
  ModelTypeEnum_TRANSACTION_FRAUD_INSIGHTS,
  ModelTypeEnum'
  #-}
