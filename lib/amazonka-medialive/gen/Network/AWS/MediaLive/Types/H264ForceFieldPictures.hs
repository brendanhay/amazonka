{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ForceFieldPictures
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264ForceFieldPictures
  ( H264ForceFieldPictures
    ( H264ForceFieldPictures'
    , H264ForceFieldPicturesDisabled
    , H264ForceFieldPicturesEnabled
    , fromH264ForceFieldPictures
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H264 Force Field Pictures
newtype H264ForceFieldPictures = H264ForceFieldPictures'{fromH264ForceFieldPictures
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern H264ForceFieldPicturesDisabled :: H264ForceFieldPictures
pattern H264ForceFieldPicturesDisabled = H264ForceFieldPictures' "DISABLED"

pattern H264ForceFieldPicturesEnabled :: H264ForceFieldPictures
pattern H264ForceFieldPicturesEnabled = H264ForceFieldPictures' "ENABLED"

{-# COMPLETE 
  H264ForceFieldPicturesDisabled,

  H264ForceFieldPicturesEnabled,
  H264ForceFieldPictures'
  #-}
