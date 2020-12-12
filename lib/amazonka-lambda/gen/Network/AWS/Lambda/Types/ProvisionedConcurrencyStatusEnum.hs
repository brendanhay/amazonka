{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
  ( ProvisionedConcurrencyStatusEnum
      ( ProvisionedConcurrencyStatusEnum',
        PCSEFailed,
        PCSEInProgress,
        PCSEReady
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProvisionedConcurrencyStatusEnum = ProvisionedConcurrencyStatusEnum' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PCSEFailed :: ProvisionedConcurrencyStatusEnum
pattern PCSEFailed = ProvisionedConcurrencyStatusEnum' "FAILED"

pattern PCSEInProgress :: ProvisionedConcurrencyStatusEnum
pattern PCSEInProgress = ProvisionedConcurrencyStatusEnum' "IN_PROGRESS"

pattern PCSEReady :: ProvisionedConcurrencyStatusEnum
pattern PCSEReady = ProvisionedConcurrencyStatusEnum' "READY"

{-# COMPLETE
  PCSEFailed,
  PCSEInProgress,
  PCSEReady,
  ProvisionedConcurrencyStatusEnum'
  #-}
