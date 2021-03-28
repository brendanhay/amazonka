{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RecorderStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.RecorderStatus
  ( RecorderStatus
    ( RecorderStatus'
    , RecorderStatusPending
    , RecorderStatusSuccess
    , RecorderStatusFailure
    , fromRecorderStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RecorderStatus = RecorderStatus'{fromRecorderStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern RecorderStatusPending :: RecorderStatus
pattern RecorderStatusPending = RecorderStatus' "Pending"

pattern RecorderStatusSuccess :: RecorderStatus
pattern RecorderStatusSuccess = RecorderStatus' "Success"

pattern RecorderStatusFailure :: RecorderStatus
pattern RecorderStatusFailure = RecorderStatus' "Failure"

{-# COMPLETE 
  RecorderStatusPending,

  RecorderStatusSuccess,

  RecorderStatusFailure,
  RecorderStatus'
  #-}
