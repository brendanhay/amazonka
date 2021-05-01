{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
  ( ProvisionedConcurrencyStatusEnum
      ( ..,
        ProvisionedConcurrencyStatusEnum_FAILED,
        ProvisionedConcurrencyStatusEnum_IN_PROGRESS,
        ProvisionedConcurrencyStatusEnum_READY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ProvisionedConcurrencyStatusEnum = ProvisionedConcurrencyStatusEnum'
  { fromProvisionedConcurrencyStatusEnum ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ProvisionedConcurrencyStatusEnum_FAILED :: ProvisionedConcurrencyStatusEnum
pattern ProvisionedConcurrencyStatusEnum_FAILED = ProvisionedConcurrencyStatusEnum' "FAILED"

pattern ProvisionedConcurrencyStatusEnum_IN_PROGRESS :: ProvisionedConcurrencyStatusEnum
pattern ProvisionedConcurrencyStatusEnum_IN_PROGRESS = ProvisionedConcurrencyStatusEnum' "IN_PROGRESS"

pattern ProvisionedConcurrencyStatusEnum_READY :: ProvisionedConcurrencyStatusEnum
pattern ProvisionedConcurrencyStatusEnum_READY = ProvisionedConcurrencyStatusEnum' "READY"

{-# COMPLETE
  ProvisionedConcurrencyStatusEnum_FAILED,
  ProvisionedConcurrencyStatusEnum_IN_PROGRESS,
  ProvisionedConcurrencyStatusEnum_READY,
  ProvisionedConcurrencyStatusEnum'
  #-}
