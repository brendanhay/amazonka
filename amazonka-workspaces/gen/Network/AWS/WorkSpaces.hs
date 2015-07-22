{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Amazon WorkSpaces Service
--
-- This is the /Amazon WorkSpaces API Reference/. This guide provides
-- detailed information about Amazon WorkSpaces operations, data types,
-- parameters, and errors.
module Network.AWS.WorkSpaces
    ( module Export
    ) where

import           Network.AWS.WorkSpaces.CreateWorkspaces             as Export
import           Network.AWS.WorkSpaces.DescribeWorkspaceBundles     as Export
import           Network.AWS.WorkSpaces.DescribeWorkspaceDirectories as Export
import           Network.AWS.WorkSpaces.DescribeWorkspaces           as Export
import           Network.AWS.WorkSpaces.RebootWorkspaces             as Export
import           Network.AWS.WorkSpaces.RebuildWorkspaces            as Export
import           Network.AWS.WorkSpaces.TerminateWorkspaces          as Export
import           Network.AWS.WorkSpaces.Types                        as Export
import           Network.AWS.WorkSpaces.Waiters                      as Export
