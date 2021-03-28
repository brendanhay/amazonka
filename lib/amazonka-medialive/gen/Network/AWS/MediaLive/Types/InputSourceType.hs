{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputSourceType
  ( InputSourceType
    ( InputSourceType'
    , InputSourceTypeStatic
    , InputSourceTypeDynamic
    , fromInputSourceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | There are two types of input sources, static and dynamic. If an input source is dynamic you can
--
-- change the source url of the input dynamically using an input switch action. However, the only input type
-- to support a dynamic url at this time is MP4_FILE. By default all input sources are static.
newtype InputSourceType = InputSourceType'{fromInputSourceType ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern InputSourceTypeStatic :: InputSourceType
pattern InputSourceTypeStatic = InputSourceType' "STATIC"

pattern InputSourceTypeDynamic :: InputSourceType
pattern InputSourceTypeDynamic = InputSourceType' "DYNAMIC"

{-# COMPLETE 
  InputSourceTypeStatic,

  InputSourceTypeDynamic,
  InputSourceType'
  #-}
