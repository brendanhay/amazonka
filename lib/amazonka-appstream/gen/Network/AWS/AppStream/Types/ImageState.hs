{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ImageState
  ( ImageState
    ( ImageState'
    , ImageStatePending
    , ImageStateAvailable
    , ImageStateFailed
    , ImageStateCopying
    , ImageStateDeleting
    , fromImageState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ImageState = ImageState'{fromImageState :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern ImageStatePending :: ImageState
pattern ImageStatePending = ImageState' "PENDING"

pattern ImageStateAvailable :: ImageState
pattern ImageStateAvailable = ImageState' "AVAILABLE"

pattern ImageStateFailed :: ImageState
pattern ImageStateFailed = ImageState' "FAILED"

pattern ImageStateCopying :: ImageState
pattern ImageStateCopying = ImageState' "COPYING"

pattern ImageStateDeleting :: ImageState
pattern ImageStateDeleting = ImageState' "DELETING"

{-# COMPLETE 
  ImageStatePending,

  ImageStateAvailable,

  ImageStateFailed,

  ImageStateCopying,

  ImageStateDeleting,
  ImageState'
  #-}
