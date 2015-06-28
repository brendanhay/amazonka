{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.DescribeScalingParameters
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

-- | Gets the scaling parameters configured for a domain. A domain\'s scaling
-- parameters specify the desired search instance type and replication
-- count. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-scaling-options.html Configuring Scaling Options>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeScalingParameters.html>
module Network.AWS.CloudSearch.DescribeScalingParameters
    (
    -- * Request
      DescribeScalingParameters
    -- ** Request constructor
    , describeScalingParameters
    -- ** Request lenses
    , dspDomainName

    -- * Response
    , DescribeScalingParametersResponse
    -- ** Response constructor
    , describeScalingParametersResponse
    -- ** Response lenses
    , dsprScalingParameters
    , dsprStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DescribeScalingParameters@
-- operation. Specifies the name of the domain you want to describe.
--
-- /See:/ 'describeScalingParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dspDomainName'
newtype DescribeScalingParameters = DescribeScalingParameters'
    { _dspDomainName :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeScalingParameters' smart constructor.
describeScalingParameters :: Text -> DescribeScalingParameters
describeScalingParameters pDomainName =
    DescribeScalingParameters'
    { _dspDomainName = pDomainName
    }

-- | FIXME: Undocumented member.
dspDomainName :: Lens' DescribeScalingParameters Text
dspDomainName = lens _dspDomainName (\ s a -> s{_dspDomainName = a});

instance AWSRequest DescribeScalingParameters where
        type Sv DescribeScalingParameters = CloudSearch
        type Rs DescribeScalingParameters =
             DescribeScalingParametersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeScalingParametersResult"
              (\ s h x ->
                 DescribeScalingParametersResponse' <$>
                   (x .@ "ScalingParameters") <*> (pure s))

instance ToHeaders DescribeScalingParameters where
        toHeaders = const mempty

instance ToPath DescribeScalingParameters where
        toPath = const "/"

instance ToQuery DescribeScalingParameters where
        toQuery DescribeScalingParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeScalingParameters" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _dspDomainName]

-- | The result of a @DescribeScalingParameters@ request. Contains the
-- scaling parameters configured for the domain specified in the request.
--
-- /See:/ 'describeScalingParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsprScalingParameters'
--
-- * 'dsprStatus'
data DescribeScalingParametersResponse = DescribeScalingParametersResponse'
    { _dsprScalingParameters :: !ScalingParametersStatus
    , _dsprStatus            :: !Status
    } deriving (Eq,Show)

-- | 'DescribeScalingParametersResponse' smart constructor.
describeScalingParametersResponse :: ScalingParametersStatus -> Status -> DescribeScalingParametersResponse
describeScalingParametersResponse pScalingParameters pStatus =
    DescribeScalingParametersResponse'
    { _dsprScalingParameters = pScalingParameters
    , _dsprStatus = pStatus
    }

-- | FIXME: Undocumented member.
dsprScalingParameters :: Lens' DescribeScalingParametersResponse ScalingParametersStatus
dsprScalingParameters = lens _dsprScalingParameters (\ s a -> s{_dsprScalingParameters = a});

-- | FIXME: Undocumented member.
dsprStatus :: Lens' DescribeScalingParametersResponse Status
dsprStatus = lens _dsprStatus (\ s a -> s{_dsprStatus = a});
