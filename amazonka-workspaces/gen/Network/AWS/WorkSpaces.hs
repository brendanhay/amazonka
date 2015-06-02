-- Module      : Network.AWS.WorkSpaces
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

-- | Amazon WorkSpaces offers you an easy way to provide a cloud-based desktop
-- experience to your end-users. You simply select from a choice of bundles that
-- offer a range of different amounts of CPU, memory, storage, and a choice of
-- applications. Users can connect from a PC, Mac desktop computer, iPad,
-- Kindle, or Android tablet.
module Network.AWS.WorkSpaces
    ( module Network.AWS.WorkSpaces.CreateWorkspaces
    , module Network.AWS.WorkSpaces.DescribeWorkspaceBundles
    , module Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
    , module Network.AWS.WorkSpaces.DescribeWorkspaces
    , module Network.AWS.WorkSpaces.RebootWorkspaces
    , module Network.AWS.WorkSpaces.RebuildWorkspaces
    , module Network.AWS.WorkSpaces.TerminateWorkspaces
    , module Network.AWS.WorkSpaces.Types
    ) where

import Network.AWS.WorkSpaces.CreateWorkspaces
import Network.AWS.WorkSpaces.DescribeWorkspaceBundles
import Network.AWS.WorkSpaces.DescribeWorkspaceDirectories
import Network.AWS.WorkSpaces.DescribeWorkspaces
import Network.AWS.WorkSpaces.RebootWorkspaces
import Network.AWS.WorkSpaces.RebuildWorkspaces
import Network.AWS.WorkSpaces.TerminateWorkspaces
import Network.AWS.WorkSpaces.Types
