{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ImageVersionStatus
  ( ImageVersionStatus
    ( ImageVersionStatus'
    , ImageVersionStatusCreating
    , ImageVersionStatusCreated
    , ImageVersionStatusCreateFailed
    , ImageVersionStatusDeleting
    , ImageVersionStatusDeleteFailed
    , fromImageVersionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ImageVersionStatus = ImageVersionStatus'{fromImageVersionStatus
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern ImageVersionStatusCreating :: ImageVersionStatus
pattern ImageVersionStatusCreating = ImageVersionStatus' "CREATING"

pattern ImageVersionStatusCreated :: ImageVersionStatus
pattern ImageVersionStatusCreated = ImageVersionStatus' "CREATED"

pattern ImageVersionStatusCreateFailed :: ImageVersionStatus
pattern ImageVersionStatusCreateFailed = ImageVersionStatus' "CREATE_FAILED"

pattern ImageVersionStatusDeleting :: ImageVersionStatus
pattern ImageVersionStatusDeleting = ImageVersionStatus' "DELETING"

pattern ImageVersionStatusDeleteFailed :: ImageVersionStatus
pattern ImageVersionStatusDeleteFailed = ImageVersionStatus' "DELETE_FAILED"

{-# COMPLETE 
  ImageVersionStatusCreating,

  ImageVersionStatusCreated,

  ImageVersionStatusCreateFailed,

  ImageVersionStatusDeleting,

  ImageVersionStatusDeleteFailed,
  ImageVersionStatus'
  #-}
