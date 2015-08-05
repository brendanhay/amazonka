{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AWS Device Farm is a service that enables mobile app developers to test
-- Android, iOS, and Fire OS apps on physical phones, tablets, and other
-- devices in the cloud.
module Network.AWS.DeviceFarm
    ( module Export
    ) where

import           Network.AWS.DeviceFarm.CreateDevicePool           as Export
import           Network.AWS.DeviceFarm.CreateProject              as Export
import           Network.AWS.DeviceFarm.CreateUpload               as Export
import           Network.AWS.DeviceFarm.GetAccountSettings         as Export
import           Network.AWS.DeviceFarm.GetDevice                  as Export
import           Network.AWS.DeviceFarm.GetDevicePool              as Export
import           Network.AWS.DeviceFarm.GetDevicePoolCompatibility as Export
import           Network.AWS.DeviceFarm.GetJob                     as Export
import           Network.AWS.DeviceFarm.GetProject                 as Export
import           Network.AWS.DeviceFarm.GetRun                     as Export
import           Network.AWS.DeviceFarm.GetSuite                   as Export
import           Network.AWS.DeviceFarm.GetTest                    as Export
import           Network.AWS.DeviceFarm.GetUpload                  as Export
import           Network.AWS.DeviceFarm.ListArtifacts              as Export
import           Network.AWS.DeviceFarm.ListDevicePools            as Export
import           Network.AWS.DeviceFarm.ListDevices                as Export
import           Network.AWS.DeviceFarm.ListJobs                   as Export
import           Network.AWS.DeviceFarm.ListProjects               as Export
import           Network.AWS.DeviceFarm.ListRuns                   as Export
import           Network.AWS.DeviceFarm.ListSamples                as Export
import           Network.AWS.DeviceFarm.ListSuites                 as Export
import           Network.AWS.DeviceFarm.ListTests                  as Export
import           Network.AWS.DeviceFarm.ListUniqueProblems         as Export
import           Network.AWS.DeviceFarm.ListUploads                as Export
import           Network.AWS.DeviceFarm.ScheduleRun                as Export
import           Network.AWS.DeviceFarm.Types                      as Export
import           Network.AWS.DeviceFarm.Waiters                    as Export
