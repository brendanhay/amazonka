{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcFormatVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.OrcFormatVersion
  ( OrcFormatVersion
    ( OrcFormatVersion'
    , OrcFormatVersionV011
    , OrcFormatVersionV012
    , fromOrcFormatVersion
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OrcFormatVersion = OrcFormatVersion'{fromOrcFormatVersion
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern OrcFormatVersionV011 :: OrcFormatVersion
pattern OrcFormatVersionV011 = OrcFormatVersion' "V0_11"

pattern OrcFormatVersionV012 :: OrcFormatVersion
pattern OrcFormatVersionV012 = OrcFormatVersion' "V0_12"

{-# COMPLETE 
  OrcFormatVersionV011,

  OrcFormatVersionV012,
  OrcFormatVersion'
  #-}
