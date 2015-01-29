{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.ListDomains
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

-- | Returns the list of domains registered in the account. The results may be
-- split into multiple pages. To retrieve subsequent pages, make the call again
-- using the nextPageToken returned by the initial call.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes. Access Control
--
-- You can use IAM policies to control this action's access to Amazon SWF
-- resources as follows:
--
-- Use a 'Resource' element with the domain name to limit the action to only
-- specified domains. The element must be set to 'arn:aws:swf::AccountID:domain/*', where
-- /AccountID/ is the account ID, with no dashes. Use an 'Action' element to allow
-- or deny permission to call this action. You cannot use an IAM policy to
-- constrain this action's parameters.  If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails. The associated event attribute's cause
-- parameter will be set to OPERATION_NOT_PERMITTED. For details and example
-- IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListDomains.html>
module Network.AWS.SWF.ListDomains
    (
    -- * Request
      ListDomains
    -- ** Request constructor
    , listDomains
    -- ** Request lenses
    , ldMaximumPageSize
    , ldNextPageToken
    , ldRegistrationStatus
    , ldReverseOrder

    -- * Response
    , ListDomainsResponse
    -- ** Response constructor
    , listDomainsResponse
    -- ** Response lenses
    , ldrDomainInfos
    , ldrNextPageToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data ListDomains = ListDomains
    { _ldMaximumPageSize    :: Maybe Nat
    , _ldNextPageToken      :: Maybe Text
    , _ldRegistrationStatus :: RegistrationStatus
    , _ldReverseOrder       :: Maybe Bool
    } deriving (Eq, Read, Show)

-- | 'ListDomains' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldMaximumPageSize' @::@ 'Maybe' 'Natural'
--
-- * 'ldNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'ldRegistrationStatus' @::@ 'RegistrationStatus'
--
-- * 'ldReverseOrder' @::@ 'Maybe' 'Bool'
--
listDomains :: RegistrationStatus -- ^ 'ldRegistrationStatus'
            -> ListDomains
listDomains p1 = ListDomains
    { _ldRegistrationStatus = p1
    , _ldNextPageToken      = Nothing
    , _ldMaximumPageSize    = Nothing
    , _ldReverseOrder       = Nothing
    }

-- | The maximum number of results that will be returned per call. 'nextPageToken'
-- can be used to obtain futher pages of results. The default is 100, which is
-- the maximum allowed page size. You can, however, specify a page size /smaller/
-- than 100.
--
-- This is an upper limit only; the actual number of results returned per call
-- may be fewer than the specified maximum.
ldMaximumPageSize :: Lens' ListDomains (Maybe Natural)
ldMaximumPageSize =
    lens _ldMaximumPageSize (\s a -> s { _ldMaximumPageSize = a })
        . mapping _Nat

-- | If a 'NextPageToken' was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again using
-- the returned token in 'nextPageToken'. Keep all other arguments unchanged.
--
-- The configured 'maximumPageSize' determines how many results can be returned
-- in a single call.
ldNextPageToken :: Lens' ListDomains (Maybe Text)
ldNextPageToken = lens _ldNextPageToken (\s a -> s { _ldNextPageToken = a })

-- | Specifies the registration status of the domains to list.
ldRegistrationStatus :: Lens' ListDomains RegistrationStatus
ldRegistrationStatus =
    lens _ldRegistrationStatus (\s a -> s { _ldRegistrationStatus = a })

-- | When set to 'true', returns the results in reverse order. By default, the
-- results are returned in ascending alphabetical order by 'name' of the domains.
ldReverseOrder :: Lens' ListDomains (Maybe Bool)
ldReverseOrder = lens _ldReverseOrder (\s a -> s { _ldReverseOrder = a })

data ListDomainsResponse = ListDomainsResponse
    { _ldrDomainInfos   :: List "domainInfos" DomainInfo
    , _ldrNextPageToken :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ListDomainsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrDomainInfos' @::@ ['DomainInfo']
--
-- * 'ldrNextPageToken' @::@ 'Maybe' 'Text'
--
listDomainsResponse :: ListDomainsResponse
listDomainsResponse = ListDomainsResponse
    { _ldrDomainInfos   = mempty
    , _ldrNextPageToken = Nothing
    }

-- | A list of DomainInfo structures.
ldrDomainInfos :: Lens' ListDomainsResponse [DomainInfo]
ldrDomainInfos = lens _ldrDomainInfos (\s a -> s { _ldrDomainInfos = a }) . _List

-- | If a 'NextPageToken' was returned by a previous call, there are more results
-- available. To retrieve the next page of results, make the call again using
-- the returned token in 'nextPageToken'. Keep all other arguments unchanged.
--
-- The configured 'maximumPageSize' determines how many results can be returned
-- in a single call.
ldrNextPageToken :: Lens' ListDomainsResponse (Maybe Text)
ldrNextPageToken = lens _ldrNextPageToken (\s a -> s { _ldrNextPageToken = a })

instance ToPath ListDomains where
    toPath = const "/"

instance ToQuery ListDomains where
    toQuery = const mempty

instance ToHeaders ListDomains

instance ToJSON ListDomains where
    toJSON ListDomains{..} = object
        [ "nextPageToken"      .= _ldNextPageToken
        , "registrationStatus" .= _ldRegistrationStatus
        , "maximumPageSize"    .= _ldMaximumPageSize
        , "reverseOrder"       .= _ldReverseOrder
        ]

instance AWSRequest ListDomains where
    type Sv ListDomains = SWF
    type Rs ListDomains = ListDomainsResponse

    request  = post "ListDomains"
    response = jsonResponse

instance FromJSON ListDomainsResponse where
    parseJSON = withObject "ListDomainsResponse" $ \o -> ListDomainsResponse
        <$> o .:? "domainInfos" .!= mempty
        <*> o .:? "nextPageToken"

instance AWSPager ListDomains where
    page rq rs
        | stop (rs ^. ldrNextPageToken) = Nothing
        | otherwise = (\x -> rq & ldNextPageToken ?~ x)
            <$> (rs ^. ldrNextPageToken)
