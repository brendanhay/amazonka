{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
  ( UpdateDataRetentionOperation
    ( UpdateDataRetentionOperation'
    , UpdateDataRetentionOperationIncreaseDataRetention
    , UpdateDataRetentionOperationDecreaseDataRetention
    , fromUpdateDataRetentionOperation
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype UpdateDataRetentionOperation = UpdateDataRetentionOperation'{fromUpdateDataRetentionOperation
                                                                     :: Core.Text}
                                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                         Core.Generic)
                                         deriving newtype (Core.IsString, Core.Hashable,
                                                           Core.NFData, Core.ToJSONKey,
                                                           Core.FromJSONKey, Core.ToJSON,
                                                           Core.FromJSON, Core.ToXML, Core.FromXML,
                                                           Core.ToText, Core.FromText,
                                                           Core.ToByteString, Core.ToQuery,
                                                           Core.ToHeader)

pattern UpdateDataRetentionOperationIncreaseDataRetention :: UpdateDataRetentionOperation
pattern UpdateDataRetentionOperationIncreaseDataRetention = UpdateDataRetentionOperation' "INCREASE_DATA_RETENTION"

pattern UpdateDataRetentionOperationDecreaseDataRetention :: UpdateDataRetentionOperation
pattern UpdateDataRetentionOperationDecreaseDataRetention = UpdateDataRetentionOperation' "DECREASE_DATA_RETENTION"

{-# COMPLETE 
  UpdateDataRetentionOperationIncreaseDataRetention,

  UpdateDataRetentionOperationDecreaseDataRetention,
  UpdateDataRetentionOperation'
  #-}
