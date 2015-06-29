{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.UpdateScalingParameters
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

-- | Configures scaling parameters for a domain. A domain\'s scaling
-- parameters specify the desired search instance type and replication
-- count. Amazon CloudSearch will still automatically scale your domain
-- based on the volume of data and traffic, but not below the desired
-- instance type and replication count. If the Multi-AZ option is enabled,
-- these values control the resources used per Availability Zone. For more
-- information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-scaling-options.html Configuring Scaling Options>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UpdateScalingParameters.html>
module Network.AWS.CloudSearch.UpdateScalingParameters
    (
    -- * Request
      UpdateScalingParameters
    -- ** Request constructor
    , updateScalingParameters
    -- ** Request lenses
    , uspDomainName
    , uspScalingParameters

    -- * Response
    , UpdateScalingParametersResponse
    -- ** Response constructor
    , updateScalingParametersResponse
    -- ** Response lenses
    , usprStatus
    , usprScalingParameters
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @UpdateScalingParameters@ operation.
-- Specifies the name of the domain you want to update and the scaling
-- parameters you want to configure.
--
-- /See:/ 'updateScalingParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uspDomainName'
--
-- * 'uspScalingParameters'
data UpdateScalingParameters = UpdateScalingParameters'
    { _uspDomainName        :: !Text
    , _uspScalingParameters :: !ScalingParameters
    } deriving (Eq,Read,Show)

-- | 'UpdateScalingParameters' smart constructor.
updateScalingParameters :: Text -> ScalingParameters -> UpdateScalingParameters
updateScalingParameters pDomainName pScalingParameters =
    UpdateScalingParameters'
    { _uspDomainName = pDomainName
    , _uspScalingParameters = pScalingParameters
    }

-- | FIXME: Undocumented member.
uspDomainName :: Lens' UpdateScalingParameters Text
uspDomainName = lens _uspDomainName (\ s a -> s{_uspDomainName = a});

-- | FIXME: Undocumented member.
uspScalingParameters :: Lens' UpdateScalingParameters ScalingParameters
uspScalingParameters = lens _uspScalingParameters (\ s a -> s{_uspScalingParameters = a});

instance AWSRequest UpdateScalingParameters where
        type Sv UpdateScalingParameters = CloudSearch
        type Rs UpdateScalingParameters =
             UpdateScalingParametersResponse
        request = post
        response
          = receiveXMLWrapper "UpdateScalingParametersResult"
              (\ s h x ->
                 UpdateScalingParametersResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ScalingParameters"))

instance ToHeaders UpdateScalingParameters where
        toHeaders = const mempty

instance ToPath UpdateScalingParameters where
        toPath = const "/"

instance ToQuery UpdateScalingParameters where
        toQuery UpdateScalingParameters'{..}
          = mconcat
              ["Action" =:
                 ("UpdateScalingParameters" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _uspDomainName,
               "ScalingParameters" =: _uspScalingParameters]

-- | The result of a @UpdateScalingParameters@ request. Contains the status
-- of the newly-configured scaling parameters.
--
-- /See:/ 'updateScalingParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usprStatus'
--
-- * 'usprScalingParameters'
data UpdateScalingParametersResponse = UpdateScalingParametersResponse'
    { _usprStatus            :: !Int
    , _usprScalingParameters :: !ScalingParametersStatus
    } deriving (Eq,Read,Show)

-- | 'UpdateScalingParametersResponse' smart constructor.
updateScalingParametersResponse :: Int -> ScalingParametersStatus -> UpdateScalingParametersResponse
updateScalingParametersResponse pStatus pScalingParameters =
    UpdateScalingParametersResponse'
    { _usprStatus = pStatus
    , _usprScalingParameters = pScalingParameters
    }

-- | FIXME: Undocumented member.
usprStatus :: Lens' UpdateScalingParametersResponse Int
usprStatus = lens _usprStatus (\ s a -> s{_usprStatus = a});

-- | FIXME: Undocumented member.
usprScalingParameters :: Lens' UpdateScalingParametersResponse ScalingParametersStatus
usprScalingParameters = lens _usprScalingParameters (\ s a -> s{_usprScalingParameters = a});
