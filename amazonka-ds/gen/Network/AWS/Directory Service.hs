-- Module      : Network.AWS.Directory Service
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AWS Directory Service
--
-- This is the /AWS Directory Service API Reference/. This guide provides
-- detailed information about AWS Directory Service operations, data types,
-- parameters, and errors.
module Network.AWS.Directory Service
    ( module Export
    ) where

import Network.AWS.Directory Service.ConnectDirectory as Export
import Network.AWS.Directory Service.CreateAlias as Export
import Network.AWS.Directory Service.CreateComputer as Export
import Network.AWS.Directory Service.CreateDirectory as Export
import Network.AWS.Directory Service.CreateSnapshot as Export
import Network.AWS.Directory Service.DeleteDirectory as Export
import Network.AWS.Directory Service.DeleteSnapshot as Export
import Network.AWS.Directory Service.DescribeDirectories as Export
import Network.AWS.Directory Service.DescribeSnapshots as Export
import Network.AWS.Directory Service.DisableRadius as Export
import Network.AWS.Directory Service.DisableSso as Export
import Network.AWS.Directory Service.EnableRadius as Export
import Network.AWS.Directory Service.EnableSso as Export
import Network.AWS.Directory Service.GetDirectoryLimits as Export
import Network.AWS.Directory Service.GetSnapshotLimits as Export
import Network.AWS.Directory Service.RestoreFromSnapshot as Export
import Network.AWS.Directory Service.Types as Export
import Network.AWS.Directory Service.UpdateRadius as Export
import Network.AWS.Directory Service.Waiters as Export
