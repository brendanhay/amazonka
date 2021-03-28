{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
  ( CloudWatchLogsInitialPosition
    ( CloudWatchLogsInitialPosition'
    , CloudWatchLogsInitialPositionStartOfFile
    , CloudWatchLogsInitialPositionEndOfFile
    , fromCloudWatchLogsInitialPosition
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. It's only used if there is no state persisted for that log stream.
newtype CloudWatchLogsInitialPosition = CloudWatchLogsInitialPosition'{fromCloudWatchLogsInitialPosition
                                                                       :: Core.Text}
                                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                          Core.Generic)
                                          deriving newtype (Core.IsString, Core.Hashable,
                                                            Core.NFData, Core.ToJSONKey,
                                                            Core.FromJSONKey, Core.ToJSON,
                                                            Core.FromJSON, Core.ToXML, Core.FromXML,
                                                            Core.ToText, Core.FromText,
                                                            Core.ToByteString, Core.ToQuery,
                                                            Core.ToHeader)

pattern CloudWatchLogsInitialPositionStartOfFile :: CloudWatchLogsInitialPosition
pattern CloudWatchLogsInitialPositionStartOfFile = CloudWatchLogsInitialPosition' "start_of_file"

pattern CloudWatchLogsInitialPositionEndOfFile :: CloudWatchLogsInitialPosition
pattern CloudWatchLogsInitialPositionEndOfFile = CloudWatchLogsInitialPosition' "end_of_file"

{-# COMPLETE 
  CloudWatchLogsInitialPositionStartOfFile,

  CloudWatchLogsInitialPositionEndOfFile,
  CloudWatchLogsInitialPosition'
  #-}
