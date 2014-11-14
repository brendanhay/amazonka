{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Route53Domains.EnableDomainAutoRenew
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation configures Amazon Route 53 to automatically renew the
-- specified domain before the domain registration expires. The cost of
-- renewing your domain registration is billed to your AWS account. The period
-- during which you can renew a domain name varies by TLD. For a list of TLDs
-- and their renewal policies, see "Renewal, restoration, and deletion times"
-- (http://wiki.gandi.net/en/domains/renew#renewal_restoration_and_deletion_times)
-- on the website for our registrar partner, Gandi. Route 53 requires that you
-- renew before the end of the renewal period that is listed on the Gandi
-- website so we can complete processing before the deadline.
module Network.AWS.Route53Domains.EnableDomainAutoRenew
    (
    -- * Request
      EnableDomainAutoRenew
    -- ** Request constructor
    , enableDomainAutoRenew
    -- ** Request lenses
    , edarDomainName

    -- * Response
    , EnableDomainAutoRenewResponse
    -- ** Response constructor
    , enableDomainAutoRenewResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53Domains.Types

newtype EnableDomainAutoRenew = EnableDomainAutoRenew
    { _edarDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'EnableDomainAutoRenew' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edarDomainName' @::@ 'Text'
--
enableDomainAutoRenew :: Text -- ^ 'edarDomainName'
                      -> EnableDomainAutoRenew
enableDomainAutoRenew p1 = EnableDomainAutoRenew
    { _edarDomainName = p1
    }

edarDomainName :: Lens' EnableDomainAutoRenew Text
edarDomainName = lens _edarDomainName (\s a -> s { _edarDomainName = a })

instance ToPath EnableDomainAutoRenew where
    toPath = const "/"

instance ToQuery EnableDomainAutoRenew where
    toQuery = const mempty

instance ToHeaders EnableDomainAutoRenew

instance ToBody EnableDomainAutoRenew where
    toBody = toBody . encode . _edarDomainName

data EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableDomainAutoRenewResponse' constructor.
enableDomainAutoRenewResponse :: EnableDomainAutoRenewResponse
enableDomainAutoRenewResponse = EnableDomainAutoRenewResponse

instance AWSRequest EnableDomainAutoRenew where
    type Sv EnableDomainAutoRenew = Route53Domains
    type Rs EnableDomainAutoRenew = EnableDomainAutoRenewResponse

    request  = post
    response = nullaryResponse EnableDomainAutoRenewResponse
