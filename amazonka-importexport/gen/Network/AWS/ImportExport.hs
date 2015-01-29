-- Module      : Network.AWS.ImportExport
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

-- | AWS Import/Export accelerates moving large amounts of data into and out of
-- AWS using portable storage devices for transport. AWS transfers your data
-- directly onto and off of storage devices using Amazon’s high-speed internal
-- network and bypassing the Internet. For significant data sets, AWS
-- Import/Export is often faster than Internet transfer and more cost effective
-- than upgrading your connectivity.
module Network.AWS.ImportExport
    ( module Network.AWS.ImportExport.CancelJob
    , module Network.AWS.ImportExport.CreateJob
    , module Network.AWS.ImportExport.GetStatus
    , module Network.AWS.ImportExport.ListJobs
    , module Network.AWS.ImportExport.Types
    , module Network.AWS.ImportExport.UpdateJob
    ) where

import Network.AWS.ImportExport.CancelJob
import Network.AWS.ImportExport.CreateJob
import Network.AWS.ImportExport.GetStatus
import Network.AWS.ImportExport.ListJobs
import Network.AWS.ImportExport.Types
import Network.AWS.ImportExport.UpdateJob
