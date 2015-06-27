{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.DeprecateDomain
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

-- | Deprecates the specified domain. After a domain has been deprecated it
-- cannot be used to create new workflow executions or register new types.
-- However, you can still use visibility actions on this domain.
-- Deprecating a domain also deprecates all activity and workflow types
-- registered in the domain. Executions that were started before the domain
-- was deprecated will continue to run.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_DeprecateDomain.html>
module Network.AWS.SWF.DeprecateDomain
    (
    -- * Request
      DeprecateDomain
    -- ** Request constructor
    , deprecateDomain
    -- ** Request lenses
    , depName

    -- * Response
    , DeprecateDomainResponse
    -- ** Response constructor
    , deprecateDomainResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'deprecateDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'depName'
newtype DeprecateDomain = DeprecateDomain'
    { _depName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeprecateDomain' smart constructor.
deprecateDomain :: Text -> DeprecateDomain
deprecateDomain pName =
    DeprecateDomain'
    { _depName = pName
    }

-- | The name of the domain to deprecate.
depName :: Lens' DeprecateDomain Text
depName = lens _depName (\ s a -> s{_depName = a});

instance AWSRequest DeprecateDomain where
        type Sv DeprecateDomain = SWF
        type Rs DeprecateDomain = DeprecateDomainResponse
        request = postJSON
        response = receiveNull DeprecateDomainResponse'

instance ToHeaders DeprecateDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.DeprecateDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DeprecateDomain where
        toJSON DeprecateDomain'{..}
          = object ["name" .= _depName]

instance ToPath DeprecateDomain where
        toPath = const "/"

instance ToQuery DeprecateDomain where
        toQuery = const mempty

-- | /See:/ 'deprecateDomainResponse' smart constructor.
data DeprecateDomainResponse =
    DeprecateDomainResponse'
    deriving (Eq,Read,Show)

-- | 'DeprecateDomainResponse' smart constructor.
deprecateDomainResponse :: DeprecateDomainResponse
deprecateDomainResponse = DeprecateDomainResponse'
