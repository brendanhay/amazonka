{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Amazon SimpleDB is a web service providing the core database functions
-- of data indexing and querying in the cloud. By offloading the time and
-- effort associated with building and operating a web-scale database,
-- SimpleDB provides developers the freedom to focus on application
-- development.
--
-- A traditional, clustered relational database requires a sizable upfront
-- capital outlay, is complex to design, and often requires extensive and
-- repetitive database administration. Amazon SimpleDB is dramatically
-- simpler, requiring no schema, automatically indexing your data and
-- providing a simple API for storage and access. This approach eliminates
-- the administrative burden of data modeling, index maintenance, and
-- performance tuning. Developers gain access to this functionality within
-- Amazon\'s proven computing environment, are able to scale instantly, and
-- pay only for what they use.
--
-- Visit <http://aws.amazon.com/simpledb/> for more information.
module Network.AWS.SDB
    ( module Export
    ) where

import           Network.AWS.SDB.BatchDeleteAttributes as Export
import           Network.AWS.SDB.BatchPutAttributes    as Export
import           Network.AWS.SDB.CreateDomain          as Export
import           Network.AWS.SDB.DeleteAttributes      as Export
import           Network.AWS.SDB.DeleteDomain          as Export
import           Network.AWS.SDB.DomainMetadata        as Export
import           Network.AWS.SDB.GetAttributes         as Export
import           Network.AWS.SDB.ListDomains           as Export
import           Network.AWS.SDB.PutAttributes         as Export
import           Network.AWS.SDB.Select                as Export
import           Network.AWS.SDB.Types                 as Export
import           Network.AWS.SDB.Waiters               as Export
