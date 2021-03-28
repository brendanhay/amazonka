{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputFilterEnable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.InputFilterEnable
  ( InputFilterEnable
    ( InputFilterEnable'
    , InputFilterEnableAuto
    , InputFilterEnableDisable
    , InputFilterEnableForce
    , fromInputFilterEnable
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify how the transcoding service applies the denoise and deblock filters. You must also enable the filters separately, with Denoise (InputDenoiseFilter) and Deblock (InputDeblockFilter). * Auto - The transcoding service determines whether to apply filtering, depending on input type and quality. * Disable - The input is not filtered. This is true even if you use the API to enable them in (InputDeblockFilter) and (InputDeblockFilter). * Force - The input is filtered regardless of input type.
newtype InputFilterEnable = InputFilterEnable'{fromInputFilterEnable
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern InputFilterEnableAuto :: InputFilterEnable
pattern InputFilterEnableAuto = InputFilterEnable' "AUTO"

pattern InputFilterEnableDisable :: InputFilterEnable
pattern InputFilterEnableDisable = InputFilterEnable' "DISABLE"

pattern InputFilterEnableForce :: InputFilterEnable
pattern InputFilterEnableForce = InputFilterEnable' "FORCE"

{-# COMPLETE 
  InputFilterEnableAuto,

  InputFilterEnableDisable,

  InputFilterEnableForce,
  InputFilterEnable'
  #-}
