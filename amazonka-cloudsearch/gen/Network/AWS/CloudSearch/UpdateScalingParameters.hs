{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateScalingParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Configures scaling parameters for a domain. A domain\'s scaling
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
    , usprsStatus
    , usprsScalingParameters
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateScalingParameters' smart constructor.
updateScalingParameters :: Text -> ScalingParameters -> UpdateScalingParameters
updateScalingParameters pDomainName_ pScalingParameters_ =
    UpdateScalingParameters'
    { _uspDomainName = pDomainName_
    , _uspScalingParameters = pScalingParameters_
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
        request = postQuery
        response
          = receiveXMLWrapper "UpdateScalingParametersResult"
              (\ s h x ->
                 UpdateScalingParametersResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ScalingParameters"))

instance ToHeaders UpdateScalingParameters where
        toHeaders = const mempty

instance ToPath UpdateScalingParameters where
        toPath = const mempty

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
-- * 'usprsStatus'
--
-- * 'usprsScalingParameters'
data UpdateScalingParametersResponse = UpdateScalingParametersResponse'
    { _usprsStatus            :: !Int
    , _usprsScalingParameters :: !ScalingParametersStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateScalingParametersResponse' smart constructor.
updateScalingParametersResponse :: Int -> ScalingParametersStatus -> UpdateScalingParametersResponse
updateScalingParametersResponse pStatus_ pScalingParameters_ =
    UpdateScalingParametersResponse'
    { _usprsStatus = pStatus_
    , _usprsScalingParameters = pScalingParameters_
    }

-- | FIXME: Undocumented member.
usprsStatus :: Lens' UpdateScalingParametersResponse Int
usprsStatus = lens _usprsStatus (\ s a -> s{_usprsStatus = a});

-- | FIXME: Undocumented member.
usprsScalingParameters :: Lens' UpdateScalingParametersResponse ScalingParametersStatus
usprsScalingParameters = lens _usprsScalingParameters (\ s a -> s{_usprsScalingParameters = a});
