-- Module      : Network.AWS.SSM
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon EC2 Simple Systems Manager (SSM) enables you to configure and manage
-- your EC2 instances. You can create a configuration document and then
-- associate it with one or more running instances.
module Network.AWS.SSM
    ( module Network.AWS.SSM.CreateAssociation
    , module Network.AWS.SSM.CreateAssociationBatch
    , module Network.AWS.SSM.CreateDocument
    , module Network.AWS.SSM.DeleteAssociation
    , module Network.AWS.SSM.DeleteDocument
    , module Network.AWS.SSM.DescribeAssociation
    , module Network.AWS.SSM.DescribeDocument
    , module Network.AWS.SSM.GetDocument
    , module Network.AWS.SSM.ListAssociations
    , module Network.AWS.SSM.ListDocuments
    , module Network.AWS.SSM.Types
    , module Network.AWS.SSM.UpdateAssociationStatus
    ) where

import Network.AWS.SSM.CreateAssociation
import Network.AWS.SSM.CreateAssociationBatch
import Network.AWS.SSM.CreateDocument
import Network.AWS.SSM.DeleteAssociation
import Network.AWS.SSM.DeleteDocument
import Network.AWS.SSM.DescribeAssociation
import Network.AWS.SSM.DescribeDocument
import Network.AWS.SSM.GetDocument
import Network.AWS.SSM.ListAssociations
import Network.AWS.SSM.ListDocuments
import Network.AWS.SSM.Types
import Network.AWS.SSM.UpdateAssociationStatus
