{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Pending
module Network.AWS.Route53Domains
    ( module Export
    ) where

import           Network.AWS.Route53Domains.CheckDomainAvailability    as Export
import           Network.AWS.Route53Domains.DeleteTagsForDomain        as Export
import           Network.AWS.Route53Domains.DisableDomainAutoRenew     as Export
import           Network.AWS.Route53Domains.DisableDomainTransferLock  as Export
import           Network.AWS.Route53Domains.EnableDomainAutoRenew      as Export
import           Network.AWS.Route53Domains.EnableDomainTransferLock   as Export
import           Network.AWS.Route53Domains.GetDomainDetail            as Export
import           Network.AWS.Route53Domains.GetOperationDetail         as Export
import           Network.AWS.Route53Domains.ListDomains                as Export
import           Network.AWS.Route53Domains.ListOperations             as Export
import           Network.AWS.Route53Domains.ListTagsForDomain          as Export
import           Network.AWS.Route53Domains.RegisterDomain             as Export
import           Network.AWS.Route53Domains.RetrieveDomainAuthCode     as Export
import           Network.AWS.Route53Domains.TransferDomain             as Export
import           Network.AWS.Route53Domains.Types                      as Export
import           Network.AWS.Route53Domains.UpdateDomainContact        as Export
import           Network.AWS.Route53Domains.UpdateDomainContactPrivacy as Export
import           Network.AWS.Route53Domains.UpdateDomainNameservers    as Export
import           Network.AWS.Route53Domains.UpdateTagsForDomain        as Export
import           Network.AWS.Route53Domains.Waiters                    as Export
