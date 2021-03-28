{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IpSetFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.IpSetFormat
  ( IpSetFormat
    ( IpSetFormat'
    , IpSetFormatTxt
    , IpSetFormatStix
    , IpSetFormatOtxCsv
    , IpSetFormatAlienVault
    , IpSetFormatProofPoint
    , IpSetFormatFireEye
    , fromIpSetFormat
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype IpSetFormat = IpSetFormat'{fromIpSetFormat :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern IpSetFormatTxt :: IpSetFormat
pattern IpSetFormatTxt = IpSetFormat' "TXT"

pattern IpSetFormatStix :: IpSetFormat
pattern IpSetFormatStix = IpSetFormat' "STIX"

pattern IpSetFormatOtxCsv :: IpSetFormat
pattern IpSetFormatOtxCsv = IpSetFormat' "OTX_CSV"

pattern IpSetFormatAlienVault :: IpSetFormat
pattern IpSetFormatAlienVault = IpSetFormat' "ALIEN_VAULT"

pattern IpSetFormatProofPoint :: IpSetFormat
pattern IpSetFormatProofPoint = IpSetFormat' "PROOF_POINT"

pattern IpSetFormatFireEye :: IpSetFormat
pattern IpSetFormatFireEye = IpSetFormat' "FIRE_EYE"

{-# COMPLETE 
  IpSetFormatTxt,

  IpSetFormatStix,

  IpSetFormatOtxCsv,

  IpSetFormatAlienVault,

  IpSetFormatProofPoint,

  IpSetFormatFireEye,
  IpSetFormat'
  #-}
