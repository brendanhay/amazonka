{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeScalingParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the scaling parameters configured for a domain. A domain\'s scaling
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
    , dsprStatus
    , dsprScalingParameters
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
                   (pure (fromEnum s)) <*> (x .@ "ScalingParameters"))

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
-- * 'dsprStatus'
--
-- * 'dsprScalingParameters'
data DescribeScalingParametersResponse = DescribeScalingParametersResponse'
    { _dsprStatus            :: !Int
    , _dsprScalingParameters :: !ScalingParametersStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingParametersResponse' smart constructor.
describeScalingParametersResponse :: Int -> ScalingParametersStatus -> DescribeScalingParametersResponse
describeScalingParametersResponse pStatus pScalingParameters =
    DescribeScalingParametersResponse'
    { _dsprStatus = pStatus
    , _dsprScalingParameters = pScalingParameters
    }

-- | FIXME: Undocumented member.
dsprStatus :: Lens' DescribeScalingParametersResponse Int
dsprStatus = lens _dsprStatus (\ s a -> s{_dsprStatus = a});

-- | FIXME: Undocumented member.
dsprScalingParameters :: Lens' DescribeScalingParametersResponse ScalingParametersStatus
dsprScalingParameters = lens _dsprScalingParameters (\ s a -> s{_dsprScalingParameters = a});
