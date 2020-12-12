{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
  ( ApplicationVersionStatus
      ( ApplicationVersionStatus',
        AVSBuilding,
        AVSFailed,
        AVSProcessed,
        AVSProcessing,
        AVSUnprocessed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ApplicationVersionStatus = ApplicationVersionStatus' Lude.Text
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

pattern AVSBuilding :: ApplicationVersionStatus
pattern AVSBuilding = ApplicationVersionStatus' "Building"

pattern AVSFailed :: ApplicationVersionStatus
pattern AVSFailed = ApplicationVersionStatus' "Failed"

pattern AVSProcessed :: ApplicationVersionStatus
pattern AVSProcessed = ApplicationVersionStatus' "Processed"

pattern AVSProcessing :: ApplicationVersionStatus
pattern AVSProcessing = ApplicationVersionStatus' "Processing"

pattern AVSUnprocessed :: ApplicationVersionStatus
pattern AVSUnprocessed = ApplicationVersionStatus' "Unprocessed"

{-# COMPLETE
  AVSBuilding,
  AVSFailed,
  AVSProcessed,
  AVSProcessing,
  AVSUnprocessed,
  ApplicationVersionStatus'
  #-}
