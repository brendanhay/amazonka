{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VolumeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.VolumeType
  ( VolumeType
    ( VolumeType'
    , VolumeTypeStandard
    , VolumeTypeGP2
    , VolumeTypeIO1
    , fromVolumeType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The type of EBS volume, standard, gp2, or io1. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
newtype VolumeType = VolumeType'{fromVolumeType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern VolumeTypeStandard :: VolumeType
pattern VolumeTypeStandard = VolumeType' "standard"

pattern VolumeTypeGP2 :: VolumeType
pattern VolumeTypeGP2 = VolumeType' "gp2"

pattern VolumeTypeIO1 :: VolumeType
pattern VolumeTypeIO1 = VolumeType' "io1"

{-# COMPLETE 
  VolumeTypeStandard,

  VolumeTypeGP2,

  VolumeTypeIO1,
  VolumeType'
  #-}
