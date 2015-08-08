{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeScalingParameters.html AWS API Reference> for DescribeScalingParameters.
module Network.AWS.CloudSearch.DescribeScalingParameters
    (
    -- * Creating a Request
      DescribeScalingParameters
    , describeScalingParameters
    -- * Request Lenses
    , dspDomainName

    -- * Destructuring the Response
    , DescribeScalingParametersResponse
    , describeScalingParametersResponse
    -- * Response Lenses
    , dsprsStatus
    , dsprsScalingParameters
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
describeScalingParameters pDomainName_ =
    DescribeScalingParameters'
    { _dspDomainName = pDomainName_
    }

-- | Undocumented member.
dspDomainName :: Lens' DescribeScalingParameters Text
dspDomainName = lens _dspDomainName (\ s a -> s{_dspDomainName = a});

instance AWSRequest DescribeScalingParameters where
        type Sv DescribeScalingParameters = CloudSearch
        type Rs DescribeScalingParameters =
             DescribeScalingParametersResponse
        request = postQuery
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
-- * 'dsprsStatus'
--
-- * 'dsprsScalingParameters'
data DescribeScalingParametersResponse = DescribeScalingParametersResponse'
    { _dsprsStatus            :: !Int
    , _dsprsScalingParameters :: !ScalingParametersStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeScalingParametersResponse' smart constructor.
describeScalingParametersResponse :: Int -> ScalingParametersStatus -> DescribeScalingParametersResponse
describeScalingParametersResponse pStatus_ pScalingParameters_ =
    DescribeScalingParametersResponse'
    { _dsprsStatus = pStatus_
    , _dsprsScalingParameters = pScalingParameters_
    }

-- | Undocumented member.
dsprsStatus :: Lens' DescribeScalingParametersResponse Int
dsprsStatus = lens _dsprsStatus (\ s a -> s{_dsprsStatus = a});

-- | Undocumented member.
dsprsScalingParameters :: Lens' DescribeScalingParametersResponse ScalingParametersStatus
dsprsScalingParameters = lens _dsprsScalingParameters (\ s a -> s{_dsprsScalingParameters = a});
