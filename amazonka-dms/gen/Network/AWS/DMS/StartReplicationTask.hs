{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task.
--
--
-- For more information about AWS DMS tasks, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks > in the /AWS Database Migration Service User Guide./
--
module Network.AWS.DMS.StartReplicationTask
    (
    -- * Creating a Request
      startReplicationTask
    , StartReplicationTask
    -- * Request Lenses
    , srtCdcStartPosition
    , srtCdcStopPosition
    , srtCdcStartTime
    , srtReplicationTaskARN
    , srtStartReplicationTaskType

    -- * Destructuring the Response
    , startReplicationTaskResponse
    , StartReplicationTaskResponse
    -- * Response Lenses
    , srtrsReplicationTask
    , srtrsResponseStatus
    ) where

import Network.AWS.DMS.Types
import Network.AWS.DMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'startReplicationTask' smart constructor.
data StartReplicationTask = StartReplicationTask'
  { _srtCdcStartPosition         :: !(Maybe Text)
  , _srtCdcStopPosition          :: !(Maybe Text)
  , _srtCdcStartTime             :: !(Maybe POSIX)
  , _srtReplicationTaskARN       :: !Text
  , _srtStartReplicationTaskType :: !StartReplicationTaskTypeValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srtCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position
