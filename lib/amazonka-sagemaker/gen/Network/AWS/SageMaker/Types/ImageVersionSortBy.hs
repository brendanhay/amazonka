{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersionSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ImageVersionSortBy
  ( ImageVersionSortBy
    ( ImageVersionSortBy'
    , ImageVersionSortByCreationTime
    , ImageVersionSortByLastModifiedTime
    , ImageVersionSortByVersion
    , fromImageVersionSortBy
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ImageVersionSortBy = ImageVersionSortBy'{fromImageVersionSortBy
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern ImageVersionSortByCreationTime :: ImageVersionSortBy
pattern ImageVersionSortByCreationTime = ImageVersionSortBy' "CREATION_TIME"

pattern ImageVersionSortByLastModifiedTime :: ImageVersionSortBy
pattern ImageVersionSortByLastModifiedTime = ImageVersionSortBy' "LAST_MODIFIED_TIME"

pattern ImageVersionSortByVersion :: ImageVersionSortBy
pattern ImageVersionSortByVersion = ImageVersionSortBy' "VERSION"

{-# COMPLETE 
  ImageVersionSortByCreationTime,

  ImageVersionSortByLastModifiedTime,

  ImageVersionSortByVersion,
  ImageVersionSortBy'
  #-}
