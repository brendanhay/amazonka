{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.MinimumProtocolVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.MinimumProtocolVersion
  ( MinimumProtocolVersion
    ( MinimumProtocolVersion'
    , MinimumProtocolVersionSSLV3
    , MinimumProtocolVersionTLSV1
    , MinimumProtocolVersionTLSV12016
    , MinimumProtocolVersionTLSV1_12016
    , MinimumProtocolVersionTLSV1_22018
    , MinimumProtocolVersionTLSV1_22019
    , fromMinimumProtocolVersion
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MinimumProtocolVersion = MinimumProtocolVersion'{fromMinimumProtocolVersion
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern MinimumProtocolVersionSSLV3 :: MinimumProtocolVersion
pattern MinimumProtocolVersionSSLV3 = MinimumProtocolVersion' "SSLv3"

pattern MinimumProtocolVersionTLSV1 :: MinimumProtocolVersion
pattern MinimumProtocolVersionTLSV1 = MinimumProtocolVersion' "TLSv1"

pattern MinimumProtocolVersionTLSV12016 :: MinimumProtocolVersion
pattern MinimumProtocolVersionTLSV12016 = MinimumProtocolVersion' "TLSv1_2016"

pattern MinimumProtocolVersionTLSV1_12016 :: MinimumProtocolVersion
pattern MinimumProtocolVersionTLSV1_12016 = MinimumProtocolVersion' "TLSv1.1_2016"

pattern MinimumProtocolVersionTLSV1_22018 :: MinimumProtocolVersion
pattern MinimumProtocolVersionTLSV1_22018 = MinimumProtocolVersion' "TLSv1.2_2018"

pattern MinimumProtocolVersionTLSV1_22019 :: MinimumProtocolVersion
pattern MinimumProtocolVersionTLSV1_22019 = MinimumProtocolVersion' "TLSv1.2_2019"

{-# COMPLETE 
  MinimumProtocolVersionSSLV3,

  MinimumProtocolVersionTLSV1,

  MinimumProtocolVersionTLSV12016,

  MinimumProtocolVersionTLSV1_12016,

  MinimumProtocolVersionTLSV1_22018,

  MinimumProtocolVersionTLSV1_22019,
  MinimumProtocolVersion'
  #-}
