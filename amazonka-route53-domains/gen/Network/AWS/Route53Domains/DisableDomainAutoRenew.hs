{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.DisableDomainAutoRenew
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation disables automatic renewal of domain registration for the
-- specified domain.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-DisableDomainAutoRenew.html>
module Network.AWS.Route53Domains.DisableDomainAutoRenew
    (
    -- * Request
      DisableDomainAutoRenew
    -- ** Request constructor
    , disableDomainAutoRenew
    -- ** Request lenses
    , ddarDomainName

    -- * Response
    , DisableDomainAutoRenewResponse
    -- ** Response constructor
    , disableDomainAutoRenewResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

newtype DisableDomainAutoRenew = DisableDomainAutoRenew
    { _ddarDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DisableDomainAutoRenew' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddarDomainName' @::@ 'Text'
--
disableDomainAutoRenew :: Text -- ^ 'ddarDomainName'
                       -> DisableDomainAutoRenew
disableDomainAutoRenew p1 = DisableDomainAutoRenew
    { _ddarDomainName = p1
    }

ddarDomainName :: Lens' DisableDomainAutoRenew Text
ddarDomainName = lens _ddarDomainName (\s a -> s { _ddarDomainName = a })

data DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisableDomainAutoRenewResponse' constructor.
disableDomainAutoRenewResponse :: DisableDomainAutoRenewResponse
disableDomainAutoRenewResponse = DisableDomainAutoRenewResponse

instance ToPath DisableDomainAutoRenew where
    toPath = const "/"

instance ToQuery DisableDomainAutoRenew where
    toQuery = const mempty

instance ToHeaders DisableDomainAutoRenew
instance ToJSON DisableDomainAutoRenew where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DisableDomainAutoRenew where
    type Sv DisableDomainAutoRenew = Route53Domains
    type Rs DisableDomainAutoRenew = DisableDomainAutoRenewResponse

    request  = post
    response = nullResponse DisableDomainAutoRenewResponse
