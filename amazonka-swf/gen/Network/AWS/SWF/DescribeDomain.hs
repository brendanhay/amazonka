{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.DescribeDomain
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

-- | Returns information about the specified domain, including description
-- and status.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DescribeDomain.html>
module Network.AWS.SWF.DescribeDomain
    (
    -- * Request
      DescribeDomain
    -- ** Request constructor
    , describeDomain
    -- ** Request lenses
    , ddName

    -- * Response
    , DescribeDomainResponse
    -- ** Response constructor
    , describeDomainResponse
    -- ** Response lenses
    , ddrStatus
    , ddrDomainInfo
    , ddrConfiguration
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'describeDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddName'
newtype DescribeDomain = DescribeDomain'
    { _ddName :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeDomain' smart constructor.
describeDomain :: Text -> DescribeDomain
describeDomain pName =
    DescribeDomain'
    { _ddName = pName
    }

-- | The name of the domain to describe.
ddName :: Lens' DescribeDomain Text
ddName = lens _ddName (\ s a -> s{_ddName = a});

instance AWSRequest DescribeDomain where
        type Sv DescribeDomain = SWF
        type Rs DescribeDomain = DescribeDomainResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "domainInfo") <*>
                     (x .:> "configuration"))

instance ToHeaders DescribeDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DescribeDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeDomain where
        toJSON DescribeDomain'{..}
          = object ["name" .= _ddName]

instance ToPath DescribeDomain where
        toPath = const "/"

instance ToQuery DescribeDomain where
        toQuery = const mempty

-- | Contains details of a domain.
--
-- /See:/ 'describeDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddrStatus'
--
-- * 'ddrDomainInfo'
--
-- * 'ddrConfiguration'
data DescribeDomainResponse = DescribeDomainResponse'
    { _ddrStatus        :: !Int
    , _ddrDomainInfo    :: !DomainInfo
    , _ddrConfiguration :: !DomainConfiguration
    } deriving (Eq,Read,Show)

-- | 'DescribeDomainResponse' smart constructor.
describeDomainResponse :: Int -> DomainInfo -> DomainConfiguration -> DescribeDomainResponse
describeDomainResponse pStatus pDomainInfo pConfiguration =
    DescribeDomainResponse'
    { _ddrStatus = pStatus
    , _ddrDomainInfo = pDomainInfo
    , _ddrConfiguration = pConfiguration
    }

-- | FIXME: Undocumented member.
ddrStatus :: Lens' DescribeDomainResponse Int
ddrStatus = lens _ddrStatus (\ s a -> s{_ddrStatus = a});

-- | FIXME: Undocumented member.
ddrDomainInfo :: Lens' DescribeDomainResponse DomainInfo
ddrDomainInfo = lens _ddrDomainInfo (\ s a -> s{_ddrDomainInfo = a});

-- | FIXME: Undocumented member.
ddrConfiguration :: Lens' DescribeDomainResponse DomainConfiguration
ddrConfiguration = lens _ddrConfiguration (\ s a -> s{_ddrConfiguration = a});
