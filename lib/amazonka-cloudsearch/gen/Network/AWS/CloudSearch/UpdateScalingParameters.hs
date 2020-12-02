{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateScalingParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures scaling parameters for a domain. A domain's scaling parameters specify the desired search instance type and replication count. Amazon CloudSearch will still automatically scale your domain based on the volume of data and traffic, but not below the desired instance type and replication count. If the Multi-AZ option is enabled, these values control the resources used per Availability Zone. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-scaling-options.html Configuring Scaling Options> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.UpdateScalingParameters
    (
    -- * Creating a Request
      updateScalingParameters
    , UpdateScalingParameters
    -- * Request Lenses
    , uspDomainName
    , uspScalingParameters

    -- * Destructuring the Response
    , updateScalingParametersResponse
    , UpdateScalingParametersResponse
    -- * Response Lenses
    , usprsResponseStatus
    , usprsScalingParameters
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'UpdateScalingParameters' @ operation. Specifies the name of the domain you want to update and the scaling parameters you want to configure.
--
--
--
-- /See:/ 'updateScalingParameters' smart constructor.
data UpdateScalingParameters = UpdateScalingParameters'
  { _uspDomainName        :: !Text
  , _uspScalingParameters :: !ScalingParameters
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateScalingParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uspDomainName' - Undocumented member.
--
-- * 'uspScalingParameters' - Undocumented member.
updateScalingParameters
    :: Text -- ^ 'uspDomainName'
    -> ScalingParameters -- ^ 'uspScalingParameters'
    -> UpdateScalingParameters
updateScalingParameters pDomainName_ pScalingParameters_ =
  UpdateScalingParameters'
    {_uspDomainName = pDomainName_, _uspScalingParameters = pScalingParameters_}


-- | Undocumented member.
uspDomainName :: Lens' UpdateScalingParameters Text
uspDomainName = lens _uspDomainName (\ s a -> s{_uspDomainName = a})

-- | Undocumented member.
uspScalingParameters :: Lens' UpdateScalingParameters ScalingParameters
uspScalingParameters = lens _uspScalingParameters (\ s a -> s{_uspScalingParameters = a})

instance AWSRequest UpdateScalingParameters where
        type Rs UpdateScalingParameters =
             UpdateScalingParametersResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "UpdateScalingParametersResult"
              (\ s h x ->
                 UpdateScalingParametersResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ScalingParameters"))

instance Hashable UpdateScalingParameters where

instance NFData UpdateScalingParameters where

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

-- | The result of a @UpdateScalingParameters@ request. Contains the status of the newly-configured scaling parameters.
--
--
--
-- /See:/ 'updateScalingParametersResponse' smart constructor.
data UpdateScalingParametersResponse = UpdateScalingParametersResponse'
  { _usprsResponseStatus    :: !Int
  , _usprsScalingParameters :: !ScalingParametersStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateScalingParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usprsResponseStatus' - -- | The response status code.
--
-- * 'usprsScalingParameters' - Undocumented member.
updateScalingParametersResponse
    :: Int -- ^ 'usprsResponseStatus'
    -> ScalingParametersStatus -- ^ 'usprsScalingParameters'
    -> UpdateScalingParametersResponse
updateScalingParametersResponse pResponseStatus_ pScalingParameters_ =
  UpdateScalingParametersResponse'
    { _usprsResponseStatus = pResponseStatus_
    , _usprsScalingParameters = pScalingParameters_
    }


-- | -- | The response status code.
usprsResponseStatus :: Lens' UpdateScalingParametersResponse Int
usprsResponseStatus = lens _usprsResponseStatus (\ s a -> s{_usprsResponseStatus = a})

-- | Undocumented member.
usprsScalingParameters :: Lens' UpdateScalingParametersResponse ScalingParametersStatus
usprsScalingParameters = lens _usprsScalingParameters (\ s a -> s{_usprsScalingParameters = a})

instance NFData UpdateScalingParametersResponse where
