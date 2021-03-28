{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.RespondToAfd
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.RespondToAfd
  ( RespondToAfd
    ( RespondToAfd'
    , RespondToAfdNone
    , RespondToAfdRespond
    , RespondToAfdPassthrough
    , fromRespondToAfd
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the video itself in response to AFD values in the input. * Choose Respond to clip the input video frame according to the AFD value, input display aspect ratio, and output display aspect ratio. * Choose Passthrough to include the input AFD values. Do not choose this when AfdSignaling is set to (NONE). A preferred implementation of this workflow is to set RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to remove all input AFD values from this output.
newtype RespondToAfd = RespondToAfd'{fromRespondToAfd :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern RespondToAfdNone :: RespondToAfd
pattern RespondToAfdNone = RespondToAfd' "NONE"

pattern RespondToAfdRespond :: RespondToAfd
pattern RespondToAfdRespond = RespondToAfd' "RESPOND"

pattern RespondToAfdPassthrough :: RespondToAfd
pattern RespondToAfdPassthrough = RespondToAfd' "PASSTHROUGH"

{-# COMPLETE 
  RespondToAfdNone,

  RespondToAfdRespond,

  RespondToAfdPassthrough,
  RespondToAfd'
  #-}
