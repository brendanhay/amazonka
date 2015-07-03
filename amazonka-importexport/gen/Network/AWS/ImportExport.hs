-- Module      : Network.AWS.ImportExport
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | AWS Import\/Export Service AWS Import\/Export accelerates transferring
-- large amounts of data between the AWS cloud and portable storage devices
-- that you mail to us. AWS Import\/Export transfers data directly onto and
-- off of your storage devices using Amazon\'s high-speed internal network
-- and bypassing the Internet. For large data sets, AWS Import\/Export is
-- often faster than Internet transfer and more cost effective than
-- upgrading your connectivity.
module Network.AWS.ImportExport
    ( module Export
    ) where

import           Network.AWS.ImportExport.CancelJob        as Export
import           Network.AWS.ImportExport.CreateJob        as Export
import           Network.AWS.ImportExport.GetShippingLabel as Export
import           Network.AWS.ImportExport.GetStatus        as Export
import           Network.AWS.ImportExport.ListJobs         as Export
import           Network.AWS.ImportExport.Types            as Export
import           Network.AWS.ImportExport.UpdateJob        as Export
import           Network.AWS.ImportExport.Waiters          as Export
