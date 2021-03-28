{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RequirePin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.RequirePin
  ( RequirePin
    ( RequirePin'
    , RequirePinYes
    , RequirePinNO
    , RequirePinOptional
    , fromRequirePin
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RequirePin = RequirePin'{fromRequirePin :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern RequirePinYes :: RequirePin
pattern RequirePinYes = RequirePin' "YES"

pattern RequirePinNO :: RequirePin
pattern RequirePinNO = RequirePin' "NO"

pattern RequirePinOptional :: RequirePin
pattern RequirePinOptional = RequirePin' "OPTIONAL"

{-# COMPLETE 
  RequirePinYes,

  RequirePinNO,

  RequirePinOptional,
  RequirePin'
  #-}
