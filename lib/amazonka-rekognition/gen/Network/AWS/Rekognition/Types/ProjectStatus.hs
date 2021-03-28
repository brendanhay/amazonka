{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ProjectStatus
  ( ProjectStatus
    ( ProjectStatus'
    , ProjectStatusCreating
    , ProjectStatusCreated
    , ProjectStatusDeleting
    , fromProjectStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProjectStatus = ProjectStatus'{fromProjectStatus ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern ProjectStatusCreating :: ProjectStatus
pattern ProjectStatusCreating = ProjectStatus' "CREATING"

pattern ProjectStatusCreated :: ProjectStatus
pattern ProjectStatusCreated = ProjectStatus' "CREATED"

pattern ProjectStatusDeleting :: ProjectStatus
pattern ProjectStatusDeleting = ProjectStatus' "DELETING"

{-# COMPLETE 
  ProjectStatusCreating,

  ProjectStatusCreated,

  ProjectStatusDeleting,
  ProjectStatus'
  #-}
