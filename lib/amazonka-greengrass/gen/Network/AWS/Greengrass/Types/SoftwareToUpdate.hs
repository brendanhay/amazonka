{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SoftwareToUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.SoftwareToUpdate
  ( SoftwareToUpdate
    ( SoftwareToUpdate'
    , SoftwareToUpdateCore
    , SoftwareToUpdateOtaAgent
    , fromSoftwareToUpdate
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The piece of software on the Greengrass core that will be updated.
newtype SoftwareToUpdate = SoftwareToUpdate'{fromSoftwareToUpdate
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern SoftwareToUpdateCore :: SoftwareToUpdate
pattern SoftwareToUpdateCore = SoftwareToUpdate' "core"

pattern SoftwareToUpdateOtaAgent :: SoftwareToUpdate
pattern SoftwareToUpdateOtaAgent = SoftwareToUpdate' "ota_agent"

{-# COMPLETE 
  SoftwareToUpdateCore,

  SoftwareToUpdateOtaAgent,
  SoftwareToUpdate'
  #-}
