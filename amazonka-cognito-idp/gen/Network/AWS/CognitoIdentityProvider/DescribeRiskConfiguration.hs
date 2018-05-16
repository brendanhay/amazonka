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
-- Module      : Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the risk configuration.
--
--
module Network.AWS.CognitoIdentityProvider.DescribeRiskConfiguration
    (
    -- * Creating a Request
      describeRiskConfiguration
    , DescribeRiskConfiguration
    -- * Request Lenses
    , drcClientId
    , drcUserPoolId

    -- * Destructuring the Response
    , describeRiskConfigurationResponse
    , DescribeRiskConfigurationResponse
    -- * Response Lenses
    , drcrsResponseStatus
    , drcrsRiskConfiguration
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRiskConfiguration' smart constructor.
data DescribeRiskConfiguration = DescribeRiskConfiguration'
  { _drcClientId   :: !(Maybe (Sensitive Text))
  , _drcUserPoolId :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRiskConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcClientId' - The app client ID.
--
-- * 'drcUserPoolId' - The user pool ID.
describeRiskConfiguration
    :: Text -- ^ 'drcUserPoolId'
    -> DescribeRiskConfiguration
describeRiskConfiguration pUserPoolId_ =
  DescribeRiskConfiguration'
    {_drcClientId = Nothing, _drcUserPoolId = pUserPoolId_}


-- | The app client ID.
drcClientId :: Lens' DescribeRiskConfiguration (Maybe Text)
drcClientId = lens _drcClientId (\ s a -> s{_drcClientId = a}) . mapping _Sensitive

-- | The user pool ID.
drcUserPoolId :: Lens' DescribeRiskConfiguration Text
drcUserPoolId = lens _drcUserPoolId (\ s a -> s{_drcUserPoolId = a})

instance AWSRequest DescribeRiskConfiguration where
        type Rs DescribeRiskConfiguration =
             DescribeRiskConfigurationResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRiskConfigurationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "RiskConfiguration"))

instance Hashable DescribeRiskConfiguration where

instance NFData DescribeRiskConfiguration where

instance ToHeaders DescribeRiskConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.DescribeRiskConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRiskConfiguration where
        toJSON DescribeRiskConfiguration'{..}
          = object
              (catMaybes
                 [("ClientId" .=) <$> _drcClientId,
                  Just ("UserPoolId" .= _drcUserPoolId)])

instance ToPath DescribeRiskConfiguration where
        toPath = const "/"

instance ToQuery DescribeRiskConfiguration where
        toQuery = const mempty

-- | /See:/ 'describeRiskConfigurationResponse' smart constructor.
data DescribeRiskConfigurationResponse = DescribeRiskConfigurationResponse'
  { _drcrsResponseStatus    :: !Int
  , _drcrsRiskConfiguration :: !RiskConfigurationType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRiskConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcrsResponseStatus' - -- | The response status code.
--
-- * 'drcrsRiskConfiguration' - The risk configuration.
describeRiskConfigurationResponse
    :: Int -- ^ 'drcrsResponseStatus'
    -> RiskConfigurationType -- ^ 'drcrsRiskConfiguration'
    -> DescribeRiskConfigurationResponse
describeRiskConfigurationResponse pResponseStatus_ pRiskConfiguration_ =
  DescribeRiskConfigurationResponse'
    { _drcrsResponseStatus = pResponseStatus_
    , _drcrsRiskConfiguration = pRiskConfiguration_
    }


-- | -- | The response status code.
drcrsResponseStatus :: Lens' DescribeRiskConfigurationResponse Int
drcrsResponseStatus = lens _drcrsResponseStatus (\ s a -> s{_drcrsResponseStatus = a})

-- | The risk configuration.
drcrsRiskConfiguration :: Lens' DescribeRiskConfigurationResponse RiskConfigurationType
drcrsRiskConfiguration = lens _drcrsRiskConfiguration (\ s a -> s{_drcrsRiskConfiguration = a})

instance NFData DescribeRiskConfigurationResponse
         where
