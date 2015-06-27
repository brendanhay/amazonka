-- Module      : Network.AWS.SSM
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

-- | Amazon EC2 Simple Systems Manager (SSM) enables you to configure and
-- manage your EC2 instances. You can create a configuration document and
-- then associate it with one or more running instances.
--
-- You can use a configuration document to automate the following tasks for
-- your Windows instances:
--
-- -   Join an AWS Directory
--
-- -   Install, repair, or uninstall software using an MSI package
--
-- -   Run PowerShell scripts
--
-- -   Configure CloudWatch Logs to monitor applications and systems
--
-- Note that configuration documents are not supported on Linux instances.
module Network.AWS.SSM
    ( module Export
    ) where

import           Network.AWS.SSM.CreateAssociation       as Export
import           Network.AWS.SSM.CreateAssociationBatch  as Export
import           Network.AWS.SSM.CreateDocument          as Export
import           Network.AWS.SSM.DeleteAssociation       as Export
import           Network.AWS.SSM.DeleteDocument          as Export
import           Network.AWS.SSM.DescribeAssociation     as Export
import           Network.AWS.SSM.DescribeDocument        as Export
import           Network.AWS.SSM.GetDocument             as Export
import           Network.AWS.SSM.ListAssociations        as Export
import           Network.AWS.SSM.ListDocuments           as Export
import           Network.AWS.SSM.Types                   as Export
import           Network.AWS.SSM.UpdateAssociationStatus as Export
import           Network.AWS.SSM.Waiters                 as Export
