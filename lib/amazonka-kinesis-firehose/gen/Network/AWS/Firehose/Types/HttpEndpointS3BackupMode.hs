{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.HttpEndpointS3BackupMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.HttpEndpointS3BackupMode
  ( HttpEndpointS3BackupMode
    ( HttpEndpointS3BackupMode'
    , HttpEndpointS3BackupModeFailedDataOnly
    , HttpEndpointS3BackupModeAllData
    , fromHttpEndpointS3BackupMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype HttpEndpointS3BackupMode = HttpEndpointS3BackupMode'{fromHttpEndpointS3BackupMode
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern HttpEndpointS3BackupModeFailedDataOnly :: HttpEndpointS3BackupMode
pattern HttpEndpointS3BackupModeFailedDataOnly = HttpEndpointS3BackupMode' "FailedDataOnly"

pattern HttpEndpointS3BackupModeAllData :: HttpEndpointS3BackupMode
pattern HttpEndpointS3BackupModeAllData = HttpEndpointS3BackupMode' "AllData"

{-# COMPLETE 
  HttpEndpointS3BackupModeFailedDataOnly,

  HttpEndpointS3BackupModeAllData,
  HttpEndpointS3BackupMode'
  #-}
