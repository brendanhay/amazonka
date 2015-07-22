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
    , usprqDomainName
    , usprqScalingParameters

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
-- * 'usprqDomainName'
--
-- * 'usprqScalingParameters'
data UpdateScalingParameters = UpdateScalingParameters'
    { _usprqDomainName        :: !Text
    , _usprqScalingParameters :: !ScalingParameters
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateScalingParameters' smart constructor.
updateScalingParameters :: Text -> ScalingParameters -> UpdateScalingParameters
updateScalingParameters pDomainName pScalingParameters =
    UpdateScalingParameters'
    { _usprqDomainName = pDomainName
    , _usprqScalingParameters = pScalingParameters
    }

-- | FIXME: Undocumented member.
usprqDomainName :: Lens' UpdateScalingParameters Text
usprqDomainName = lens _usprqDomainName (\ s a -> s{_usprqDomainName = a});

-- | FIXME: Undocumented member.
usprqScalingParameters :: Lens' UpdateScalingParameters ScalingParameters
usprqScalingParameters = lens _usprqScalingParameters (\ s a -> s{_usprqScalingParameters = a});

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
               "DomainName" =: _usprqDomainName,
               "ScalingParameters" =: _usprqScalingParameters]

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
updateScalingParametersResponse pStatus pScalingParameters =
    UpdateScalingParametersResponse'
    { _usprsStatus = pStatus
    , _usprsScalingParameters = pScalingParameters
    }

-- | FIXME: Undocumented member.
usprsStatus :: Lens' UpdateScalingParametersResponse Int
usprsStatus = lens _usprsStatus (\ s a -> s{_usprsStatus = a});

-- | FIXME: Undocumented member.
usprsScalingParameters :: Lens' UpdateScalingParametersResponse ScalingParametersStatus
usprsScalingParameters = lens _usprsScalingParameters (\ s a -> s{_usprsScalingParameters = a});
