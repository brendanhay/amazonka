{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AvcIntraClass
  ( AvcIntraClass
    ( AvcIntraClass'
    , AvcIntraClassClass50
    , AvcIntraClassClass100
    , AvcIntraClassClass200
    , fromAvcIntraClass
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify the AVC-Intra class of your output. The AVC-Intra class selection determines the output video bit rate depending on the frame rate of the output. Outputs with higher class values have higher bitrates and improved image quality.
newtype AvcIntraClass = AvcIntraClass'{fromAvcIntraClass ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern AvcIntraClassClass50 :: AvcIntraClass
pattern AvcIntraClassClass50 = AvcIntraClass' "CLASS_50"

pattern AvcIntraClassClass100 :: AvcIntraClass
pattern AvcIntraClassClass100 = AvcIntraClass' "CLASS_100"

pattern AvcIntraClassClass200 :: AvcIntraClass
pattern AvcIntraClassClass200 = AvcIntraClass' "CLASS_200"

{-# COMPLETE 
  AvcIntraClassClass50,

  AvcIntraClassClass100,

  AvcIntraClassClass200,
  AvcIntraClass'
  #-}
