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
-- Module      : Network.AWS.DMS.CreateReplicationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication task using the specified parameters.
--
--
module Network.AWS.DMS.CreateReplicationTask
    (
    -- * Creating a Request
      createReplicationTask
    , CreateReplicationTask
    -- * Request Lenses
    , crtReplicationTaskSettings
    , crtCdcStartPosition
    , crtCdcStopPosition
    , crtTags
    , crtCdcStartTime
    , crtReplicationTaskIdentifier
    , crtSourceEndpointARN
    , crtTargetEndpointARN
    , crtReplicationInstanceARN
    , crtMigrationType
    , crtTableMappings

    -- * Destructuring the Response
    , createReplicationTaskResponse
    , CreateReplicationTaskResponse
    -- * Response Lenses
    , crtrsReplicationTask
    , crtrsResponseStatus
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
-- /See:/ 'createReplicationTask' smart constructor.
data CreateReplicationTask = CreateReplicationTask'
  { _crtReplicationTaskSettings   :: !(Maybe Text)
  , _crtCdcStartPosition          :: !(Maybe Text)
  , _crtCdcStopPosition           :: !(Maybe Text)
  , _crtTags                      :: !(Maybe [Tag])
  , _crtCdcStartTime              :: !(Maybe POSIX)
  , _crtReplicationTaskIdentifier :: !Text
  , _crtSourceEndpointARN         :: !Text
  , _crtTargetEndpointARN         :: !Text
  , _crtReplicationInstanceARN    :: !Text
  , _crtMigrationType             :: !MigrationTypeValue
  , _crtTableMappings             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crtReplicationTaskSettings' - Settings for the task, such as target metadata settings. For a complete list of task settings, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Task Settings for AWS Database Migration Service Tasks> in the /AWS Database Migration User Guide./
--
-- * 'crtCdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error. The value can be in date, checkpoint, or LSN/SCN format. Date Example: --cdc-start-position
