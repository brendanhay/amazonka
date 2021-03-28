{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.VolumeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.VolumeType
  ( VolumeType
    ( VolumeType'
    , VolumeTypeGP2
    , VolumeTypeIO1
    , VolumeTypeStandard
    , fromVolumeType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype VolumeType = VolumeType'{fromVolumeType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern VolumeTypeGP2 :: VolumeType
pattern VolumeTypeGP2 = VolumeType' "gp2"

pattern VolumeTypeIO1 :: VolumeType
pattern VolumeTypeIO1 = VolumeType' "io1"

pattern VolumeTypeStandard :: VolumeType
pattern VolumeTypeStandard = VolumeType' "standard"

{-# COMPLETE 
  VolumeTypeGP2,

  VolumeTypeIO1,

  VolumeTypeStandard,
  VolumeType'
  #-}
