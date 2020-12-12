{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
  ( UpdateDataRetentionOperation
      ( UpdateDataRetentionOperation',
        DecreaseDataRetention,
        IncreaseDataRetention
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UpdateDataRetentionOperation = UpdateDataRetentionOperation' Lude.Text
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

pattern DecreaseDataRetention :: UpdateDataRetentionOperation
pattern DecreaseDataRetention = UpdateDataRetentionOperation' "DECREASE_DATA_RETENTION"

pattern IncreaseDataRetention :: UpdateDataRetentionOperation
pattern IncreaseDataRetention = UpdateDataRetentionOperation' "INCREASE_DATA_RETENTION"

{-# COMPLETE
  DecreaseDataRetention,
  IncreaseDataRetention,
  UpdateDataRetentionOperation'
  #-}
