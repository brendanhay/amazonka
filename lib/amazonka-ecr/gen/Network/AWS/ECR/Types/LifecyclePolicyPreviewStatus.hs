{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus
  ( LifecyclePolicyPreviewStatus
      ( LifecyclePolicyPreviewStatus',
        LPPSComplete,
        LPPSExpired,
        LPPSFailed,
        LPPSInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LifecyclePolicyPreviewStatus = LifecyclePolicyPreviewStatus' Lude.Text
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

pattern LPPSComplete :: LifecyclePolicyPreviewStatus
pattern LPPSComplete = LifecyclePolicyPreviewStatus' "COMPLETE"

pattern LPPSExpired :: LifecyclePolicyPreviewStatus
pattern LPPSExpired = LifecyclePolicyPreviewStatus' "EXPIRED"

pattern LPPSFailed :: LifecyclePolicyPreviewStatus
pattern LPPSFailed = LifecyclePolicyPreviewStatus' "FAILED"

pattern LPPSInProgress :: LifecyclePolicyPreviewStatus
pattern LPPSInProgress = LifecyclePolicyPreviewStatus' "IN_PROGRESS"

{-# COMPLETE
  LPPSComplete,
  LPPSExpired,
  LPPSFailed,
  LPPSInProgress,
  LifecyclePolicyPreviewStatus'
  #-}
