{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DetailedModelPackageStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DetailedModelPackageStatus
  ( DetailedModelPackageStatus
      ( DetailedModelPackageStatus',
        DMPSCompleted,
        DMPSFailed,
        DMPSInProgress,
        DMPSNotStarted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DetailedModelPackageStatus = DetailedModelPackageStatus' Lude.Text
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

pattern DMPSCompleted :: DetailedModelPackageStatus
pattern DMPSCompleted = DetailedModelPackageStatus' "Completed"

pattern DMPSFailed :: DetailedModelPackageStatus
pattern DMPSFailed = DetailedModelPackageStatus' "Failed"

pattern DMPSInProgress :: DetailedModelPackageStatus
pattern DMPSInProgress = DetailedModelPackageStatus' "InProgress"

pattern DMPSNotStarted :: DetailedModelPackageStatus
pattern DMPSNotStarted = DetailedModelPackageStatus' "NotStarted"

{-# COMPLETE
  DMPSCompleted,
  DMPSFailed,
  DMPSInProgress,
  DMPSNotStarted,
  DetailedModelPackageStatus'
  #-}
