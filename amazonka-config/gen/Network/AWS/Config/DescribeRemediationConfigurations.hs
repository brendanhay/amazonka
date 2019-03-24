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
-- Module      : Network.AWS.Config.DescribeRemediationConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of one or more remediation configurations.
--
--
module Network.AWS.Config.DescribeRemediationConfigurations
    (
    -- * Creating a Request
      describeRemediationConfigurations
    , DescribeRemediationConfigurations
    -- * Request Lenses
    , drcConfigRuleNames

    -- * Destructuring the Response
    , describeRemediationConfigurationsResponse
    , DescribeRemediationConfigurationsResponse
    -- * Response Lenses
    , drcsrsRemediationConfigurations
    , drcsrsResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeRemediationConfigurations' smart constructor.
newtype DescribeRemediationConfigurations = DescribeRemediationConfigurations'
  { _drcConfigRuleNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRemediationConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcConfigRuleNames' - A list of AWS Config rule names of remediation configurations for which you want details.
describeRemediationConfigurations
    :: DescribeRemediationConfigurations
describeRemediationConfigurations =
  DescribeRemediationConfigurations' {_drcConfigRuleNames = mempty}


-- | A list of AWS Config rule names of remediation configurations for which you want details.
drcConfigRuleNames :: Lens' DescribeRemediationConfigurations [Text]
drcConfigRuleNames = lens _drcConfigRuleNames (\ s a -> s{_drcConfigRuleNames = a}) . _Coerce

instance AWSRequest DescribeRemediationConfigurations
         where
        type Rs DescribeRemediationConfigurations =
             DescribeRemediationConfigurationsResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRemediationConfigurationsResponse' <$>
                   (x .?> "RemediationConfigurations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeRemediationConfigurations
         where

instance NFData DescribeRemediationConfigurations
         where

instance ToHeaders DescribeRemediationConfigurations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DescribeRemediationConfigurations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRemediationConfigurations
         where
        toJSON DescribeRemediationConfigurations'{..}
          = object
              (catMaybes
                 [Just ("ConfigRuleNames" .= _drcConfigRuleNames)])

instance ToPath DescribeRemediationConfigurations
         where
        toPath = const "/"

instance ToQuery DescribeRemediationConfigurations
         where
        toQuery = const mempty

-- | /See:/ 'describeRemediationConfigurationsResponse' smart constructor.
data DescribeRemediationConfigurationsResponse = DescribeRemediationConfigurationsResponse'
  { _drcsrsRemediationConfigurations :: !(Maybe [RemediationConfiguration])
  , _drcsrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeRemediationConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcsrsRemediationConfigurations' - Returns a remediation configuration object.
--
-- * 'drcsrsResponseStatus' - -- | The response status code.
describeRemediationConfigurationsResponse
    :: Int -- ^ 'drcsrsResponseStatus'
    -> DescribeRemediationConfigurationsResponse
describeRemediationConfigurationsResponse pResponseStatus_ =
  DescribeRemediationConfigurationsResponse'
    { _drcsrsRemediationConfigurations = Nothing
    , _drcsrsResponseStatus = pResponseStatus_
    }


-- | Returns a remediation configuration object.
drcsrsRemediationConfigurations :: Lens' DescribeRemediationConfigurationsResponse [RemediationConfiguration]
drcsrsRemediationConfigurations = lens _drcsrsRemediationConfigurations (\ s a -> s{_drcsrsRemediationConfigurations = a}) . _Default . _Coerce

-- | -- | The response status code.
drcsrsResponseStatus :: Lens' DescribeRemediationConfigurationsResponse Int
drcsrsResponseStatus = lens _drcsrsResponseStatus (\ s a -> s{_drcsrsResponseStatus = a})

instance NFData
           DescribeRemediationConfigurationsResponse
         where
