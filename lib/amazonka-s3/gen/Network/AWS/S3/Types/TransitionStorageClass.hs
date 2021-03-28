{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.TransitionStorageClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.TransitionStorageClass
  ( TransitionStorageClass
    ( TransitionStorageClass'
    , TransitionStorageClassGlacier
    , TransitionStorageClassStandardIa
    , TransitionStorageClassOnezoneIa
    , TransitionStorageClassIntelligentTiering
    , TransitionStorageClassDeepArchive
    , fromTransitionStorageClass
    )
  ) where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

newtype TransitionStorageClass = TransitionStorageClass'{fromTransitionStorageClass
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern TransitionStorageClassGlacier :: TransitionStorageClass
pattern TransitionStorageClassGlacier = TransitionStorageClass' "GLACIER"

pattern TransitionStorageClassStandardIa :: TransitionStorageClass
pattern TransitionStorageClassStandardIa = TransitionStorageClass' "STANDARD_IA"

pattern TransitionStorageClassOnezoneIa :: TransitionStorageClass
pattern TransitionStorageClassOnezoneIa = TransitionStorageClass' "ONEZONE_IA"

pattern TransitionStorageClassIntelligentTiering :: TransitionStorageClass
pattern TransitionStorageClassIntelligentTiering = TransitionStorageClass' "INTELLIGENT_TIERING"

pattern TransitionStorageClassDeepArchive :: TransitionStorageClass
pattern TransitionStorageClassDeepArchive = TransitionStorageClass' "DEEP_ARCHIVE"

{-# COMPLETE 
  TransitionStorageClassGlacier,

  TransitionStorageClassStandardIa,

  TransitionStorageClassOnezoneIa,

  TransitionStorageClassIntelligentTiering,

  TransitionStorageClassDeepArchive,
  TransitionStorageClass'
  #-}
