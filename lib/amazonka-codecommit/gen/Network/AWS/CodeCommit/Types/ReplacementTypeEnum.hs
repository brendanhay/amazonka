{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ReplacementTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ReplacementTypeEnum
  ( ReplacementTypeEnum
    ( ReplacementTypeEnum'
    , ReplacementTypeEnumKeepBase
    , ReplacementTypeEnumKeepSource
    , ReplacementTypeEnumKeepDestination
    , ReplacementTypeEnumUseNewContent
    , fromReplacementTypeEnum
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ReplacementTypeEnum = ReplacementTypeEnum'{fromReplacementTypeEnum
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern ReplacementTypeEnumKeepBase :: ReplacementTypeEnum
pattern ReplacementTypeEnumKeepBase = ReplacementTypeEnum' "KEEP_BASE"

pattern ReplacementTypeEnumKeepSource :: ReplacementTypeEnum
pattern ReplacementTypeEnumKeepSource = ReplacementTypeEnum' "KEEP_SOURCE"

pattern ReplacementTypeEnumKeepDestination :: ReplacementTypeEnum
pattern ReplacementTypeEnumKeepDestination = ReplacementTypeEnum' "KEEP_DESTINATION"

pattern ReplacementTypeEnumUseNewContent :: ReplacementTypeEnum
pattern ReplacementTypeEnumUseNewContent = ReplacementTypeEnum' "USE_NEW_CONTENT"

{-# COMPLETE 
  ReplacementTypeEnumKeepBase,

  ReplacementTypeEnumKeepSource,

  ReplacementTypeEnumKeepDestination,

  ReplacementTypeEnumUseNewContent,
  ReplacementTypeEnum'
  #-}
