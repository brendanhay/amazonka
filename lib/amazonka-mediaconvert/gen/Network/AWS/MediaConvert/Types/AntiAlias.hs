{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AntiAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AntiAlias
  ( AntiAlias
    ( AntiAlias'
    , AntiAliasDisabled
    , AntiAliasEnabled
    , fromAntiAlias
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The anti-alias filter is automatically applied to all outputs. The service no longer accepts the value DISABLED for AntiAlias. If you specify that in your job, the service will ignore the setting.
newtype AntiAlias = AntiAlias'{fromAntiAlias :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern AntiAliasDisabled :: AntiAlias
pattern AntiAliasDisabled = AntiAlias' "DISABLED"

pattern AntiAliasEnabled :: AntiAlias
pattern AntiAliasEnabled = AntiAlias' "ENABLED"

{-# COMPLETE 
  AntiAliasDisabled,

  AntiAliasEnabled,
  AntiAlias'
  #-}
