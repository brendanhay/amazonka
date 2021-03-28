{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsCcDescriptor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsCcDescriptor
  ( M2tsCcDescriptor
    ( M2tsCcDescriptor'
    , M2tsCcDescriptorDisabled
    , M2tsCcDescriptorEnabled
    , fromM2tsCcDescriptor
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | M2ts Cc Descriptor
newtype M2tsCcDescriptor = M2tsCcDescriptor'{fromM2tsCcDescriptor
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern M2tsCcDescriptorDisabled :: M2tsCcDescriptor
pattern M2tsCcDescriptorDisabled = M2tsCcDescriptor' "DISABLED"

pattern M2tsCcDescriptorEnabled :: M2tsCcDescriptor
pattern M2tsCcDescriptorEnabled = M2tsCcDescriptor' "ENABLED"

{-# COMPLETE 
  M2tsCcDescriptorDisabled,

  M2tsCcDescriptorEnabled,
  M2tsCcDescriptor'
  #-}
