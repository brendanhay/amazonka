{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.RIProductDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.RIProductDescription
  ( RIProductDescription
    ( RIProductDescription'
    , RIProductDescriptionLinuxUnix
    , RIProductDescriptionLinuxUnixAmazonVpc
    , RIProductDescriptionWindows
    , RIProductDescriptionWindowsAmazonVpc
    , fromRIProductDescription
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RIProductDescription = RIProductDescription'{fromRIProductDescription
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern RIProductDescriptionLinuxUnix :: RIProductDescription
pattern RIProductDescriptionLinuxUnix = RIProductDescription' "Linux/UNIX"

pattern RIProductDescriptionLinuxUnixAmazonVpc :: RIProductDescription
pattern RIProductDescriptionLinuxUnixAmazonVpc = RIProductDescription' "Linux/UNIX (Amazon VPC)"

pattern RIProductDescriptionWindows :: RIProductDescription
pattern RIProductDescriptionWindows = RIProductDescription' "Windows"

pattern RIProductDescriptionWindowsAmazonVpc :: RIProductDescription
pattern RIProductDescriptionWindowsAmazonVpc = RIProductDescription' "Windows (Amazon VPC)"

{-# COMPLETE 
  RIProductDescriptionLinuxUnix,

  RIProductDescriptionLinuxUnixAmazonVpc,

  RIProductDescriptionWindows,

  RIProductDescriptionWindowsAmazonVpc,
  RIProductDescription'
  #-}
