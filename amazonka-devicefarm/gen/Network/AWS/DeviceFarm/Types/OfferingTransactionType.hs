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
-- Module      : Network.AWS.DeviceFarm.Types.OfferingTransactionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingTransactionType
  ( OfferingTransactionType
      ( ..,
        OfferingTransactionType_PURCHASE,
        OfferingTransactionType_RENEW,
        OfferingTransactionType_SYSTEM
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OfferingTransactionType = OfferingTransactionType'
  { fromOfferingTransactionType ::
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

pattern OfferingTransactionType_PURCHASE :: OfferingTransactionType
pattern OfferingTransactionType_PURCHASE = OfferingTransactionType' "PURCHASE"

pattern OfferingTransactionType_RENEW :: OfferingTransactionType
pattern OfferingTransactionType_RENEW = OfferingTransactionType' "RENEW"

pattern OfferingTransactionType_SYSTEM :: OfferingTransactionType
pattern OfferingTransactionType_SYSTEM = OfferingTransactionType' "SYSTEM"

{-# COMPLETE
  OfferingTransactionType_PURCHASE,
  OfferingTransactionType_RENEW,
  OfferingTransactionType_SYSTEM,
  OfferingTransactionType'
  #-}
