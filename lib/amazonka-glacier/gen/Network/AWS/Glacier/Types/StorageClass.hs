{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.StorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glacier.Types.StorageClass
  ( StorageClass
    ( StorageClass'
    , StorageClassStandard
    , StorageClassReducedRedundancy
    , StorageClassStandardIa
    , fromStorageClass
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype StorageClass = StorageClass'{fromStorageClass :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern StorageClassStandard :: StorageClass
pattern StorageClassStandard = StorageClass' "STANDARD"

pattern StorageClassReducedRedundancy :: StorageClass
pattern StorageClassReducedRedundancy = StorageClass' "REDUCED_REDUNDANCY"

pattern StorageClassStandardIa :: StorageClass
pattern StorageClassStandardIa = StorageClass' "STANDARD_IA"

{-# COMPLETE 
  StorageClassStandard,

  StorageClassReducedRedundancy,

  StorageClassStandardIa,
  StorageClass'
  #-}
