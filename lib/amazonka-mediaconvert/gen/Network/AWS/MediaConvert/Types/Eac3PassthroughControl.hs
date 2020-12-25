{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3PassthroughControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3PassthroughControl
  ( Eac3PassthroughControl
      ( Eac3PassthroughControl',
        Eac3PassthroughControlWhenPossible,
        Eac3PassthroughControlNoPassthrough,
        fromEac3PassthroughControl
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
newtype Eac3PassthroughControl = Eac3PassthroughControl'
  { fromEac3PassthroughControl ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Eac3PassthroughControlWhenPossible :: Eac3PassthroughControl
pattern Eac3PassthroughControlWhenPossible = Eac3PassthroughControl' "WHEN_POSSIBLE"

pattern Eac3PassthroughControlNoPassthrough :: Eac3PassthroughControl
pattern Eac3PassthroughControlNoPassthrough = Eac3PassthroughControl' "NO_PASSTHROUGH"

{-# COMPLETE
  Eac3PassthroughControlWhenPossible,
  Eac3PassthroughControlNoPassthrough,
  Eac3PassthroughControl'
  #-}
