{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.VolumeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.VolumeType
  ( VolumeType
      ( VolumeType',
        Standard,
        GP2,
        IO1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The type of EBS volume, standard, gp2, or io1. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-createupdatedomains.html#es-createdomain-configure-ebs Configuring EBS-based Storage> for more information.
newtype VolumeType = VolumeType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Standard :: VolumeType
pattern Standard = VolumeType' "standard"

pattern GP2 :: VolumeType
pattern GP2 = VolumeType' "gp2"

pattern IO1 :: VolumeType
pattern IO1 = VolumeType' "io1"

{-# COMPLETE
  Standard,
  GP2,
  IO1,
  VolumeType'
  #-}
