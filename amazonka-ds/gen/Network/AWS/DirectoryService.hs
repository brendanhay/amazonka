{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service
--
-- This is the /AWS Directory Service API Reference/. This guide provides
-- detailed information about AWS Directory Service operations, data types,
-- parameters, and errors.
module Network.AWS.DirectoryService
    ( module Export
    ) where

import           Network.AWS.DirectoryService.ConnectDirectory    as Export
import           Network.AWS.DirectoryService.CreateAlias         as Export
import           Network.AWS.DirectoryService.CreateComputer      as Export
import           Network.AWS.DirectoryService.CreateDirectory     as Export
import           Network.AWS.DirectoryService.CreateSnapshot      as Export
import           Network.AWS.DirectoryService.DeleteDirectory     as Export
import           Network.AWS.DirectoryService.DeleteSnapshot      as Export
import           Network.AWS.DirectoryService.DescribeDirectories as Export
import           Network.AWS.DirectoryService.DescribeSnapshots   as Export
import           Network.AWS.DirectoryService.DisableRadius       as Export
import           Network.AWS.DirectoryService.DisableSso          as Export
import           Network.AWS.DirectoryService.EnableRadius        as Export
import           Network.AWS.DirectoryService.EnableSso           as Export
import           Network.AWS.DirectoryService.GetDirectoryLimits  as Export
import           Network.AWS.DirectoryService.GetSnapshotLimits   as Export
import           Network.AWS.DirectoryService.RestoreFromSnapshot as Export
import           Network.AWS.DirectoryService.Types               as Export
import           Network.AWS.DirectoryService.Types.Product       as Export
import           Network.AWS.DirectoryService.Types.Sum           as Export
import           Network.AWS.DirectoryService.UpdateRadius        as Export
import           Network.AWS.DirectoryService.Waiters             as Export
