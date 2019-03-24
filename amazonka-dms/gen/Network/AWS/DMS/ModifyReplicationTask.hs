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
-- Module      : Network.AWS.DMS.ModifyReplicationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified replication task.
--
--
-- You can't modify the task endpoints. The task must be stopped before you can modify it.
--
-- For more information about AWS DMS tasks, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks> in the /AWS Database Migration Service User Guide/ .
--
module Network.AWS.DMS.ModifyReplicationTask
    (
    -- * Creating a Request
      modifyReplicationTask
    , ModifyReplicationTask
    -- * Request Lenses
    , mrtReplicationTaskSettings
    , mrtReplicationTaskIdentifier
    , mrtCdcStartPosition
    , mrtTableMappings
    , mrtMigrationType
    , mrtCdcStopPosition
    , mrtCdcStartTime
    , mrtReplicationTaskARN

    -- * Destructuring the Response
    , modifyReplicationTaskResponse
    , ModifyReplicationTaskResponse
    -- * Response Lenses
    , mrtrsReplicationTask
    , mrtrsResponseStatus
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
-- /See:/ 'modifyReplicationTask' smart constructor.
data ModifyReplicationTask = ModifyReplicationTask'
  { _mrtReplicationTaskSettings   :: !(Maybe Text)
  , _mrtReplicationTaskIdentifier :: !(Maybe Text)
  , _mrtCdcStartPosition          :: !(Maybe Text)
  , _mrtTableMappings             :: !(Maybe Text)
  , _mrtMigrationType             :: !(Maybe MigrationTypeValue)
  , _mrtCdcStopPosition           :: !(Maybe Text)
  , _mrtCdcStartTime              :: !(Maybe POSIX)
  , _mrtReplicationTaskARN        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrtReplicationTaskSettings' - JSON file that contains settings for the task, such as target metadata settings.
--
-- * 'mrtReplicationTaskIdentifier' - The replication task identifier. Constraints:     * Must contain from 1 to 255 alphanumeric characters or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'mrtCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position
