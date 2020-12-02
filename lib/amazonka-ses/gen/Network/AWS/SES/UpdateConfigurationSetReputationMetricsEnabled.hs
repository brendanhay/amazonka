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
-- Module      : Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables or disables the publishing of reputation metrics for emails sent using a specific configuration set. Reputation metrics include bounce and complaint rates. These metrics are published to Amazon CloudWatch. By using Amazon CloudWatch, you can create alarms when bounce or complaint rates exceed a certain threshold.
--
--
-- You can execute this operation no more than once per second.
--
module Network.AWS.SES.UpdateConfigurationSetReputationMetricsEnabled
    (
    -- * Creating a Request
      updateConfigurationSetReputationMetricsEnabled
    , UpdateConfigurationSetReputationMetricsEnabled
    -- * Request Lenses
    , ucsrmeConfigurationSetName
    , ucsrmeEnabled

    -- * Destructuring the Response
    , updateConfigurationSetReputationMetricsEnabledResponse
    , UpdateConfigurationSetReputationMetricsEnabledResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | Represents a request to modify the reputation metric publishing settings for a configuration set.
--
--
--
-- /See:/ 'updateConfigurationSetReputationMetricsEnabled' smart constructor.
data UpdateConfigurationSetReputationMetricsEnabled = UpdateConfigurationSetReputationMetricsEnabled'
  { _ucsrmeConfigurationSetName :: !Text
  , _ucsrmeEnabled              :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationSetReputationMetricsEnabled' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucsrmeConfigurationSetName' - The name of the configuration set that you want to update.
--
-- * 'ucsrmeEnabled' - Describes whether or not Amazon SES will publish reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
updateConfigurationSetReputationMetricsEnabled
    :: Text -- ^ 'ucsrmeConfigurationSetName'
    -> Bool -- ^ 'ucsrmeEnabled'
    -> UpdateConfigurationSetReputationMetricsEnabled
updateConfigurationSetReputationMetricsEnabled pConfigurationSetName_ pEnabled_ =
  UpdateConfigurationSetReputationMetricsEnabled'
    { _ucsrmeConfigurationSetName = pConfigurationSetName_
    , _ucsrmeEnabled = pEnabled_
    }


-- | The name of the configuration set that you want to update.
ucsrmeConfigurationSetName :: Lens' UpdateConfigurationSetReputationMetricsEnabled Text
ucsrmeConfigurationSetName = lens _ucsrmeConfigurationSetName (\ s a -> s{_ucsrmeConfigurationSetName = a})

-- | Describes whether or not Amazon SES will publish reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
ucsrmeEnabled :: Lens' UpdateConfigurationSetReputationMetricsEnabled Bool
ucsrmeEnabled = lens _ucsrmeEnabled (\ s a -> s{_ucsrmeEnabled = a})

instance AWSRequest
           UpdateConfigurationSetReputationMetricsEnabled
         where
        type Rs
               UpdateConfigurationSetReputationMetricsEnabled
             =
             UpdateConfigurationSetReputationMetricsEnabledResponse
        request = postQuery ses
        response
          = receiveNull
              UpdateConfigurationSetReputationMetricsEnabledResponse'

instance Hashable
           UpdateConfigurationSetReputationMetricsEnabled
         where

instance NFData
           UpdateConfigurationSetReputationMetricsEnabled
         where

instance ToHeaders
           UpdateConfigurationSetReputationMetricsEnabled
         where
        toHeaders = const mempty

instance ToPath
           UpdateConfigurationSetReputationMetricsEnabled
         where
        toPath = const "/"

instance ToQuery
           UpdateConfigurationSetReputationMetricsEnabled
         where
        toQuery
          UpdateConfigurationSetReputationMetricsEnabled'{..}
          = mconcat
              ["Action" =:
                 ("UpdateConfigurationSetReputationMetricsEnabled" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ConfigurationSetName" =:
                 _ucsrmeConfigurationSetName,
               "Enabled" =: _ucsrmeEnabled]

-- | /See:/ 'updateConfigurationSetReputationMetricsEnabledResponse' smart constructor.
data UpdateConfigurationSetReputationMetricsEnabledResponse =
  UpdateConfigurationSetReputationMetricsEnabledResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConfigurationSetReputationMetricsEnabledResponse' with the minimum fields required to make a request.
--
updateConfigurationSetReputationMetricsEnabledResponse
    :: UpdateConfigurationSetReputationMetricsEnabledResponse
updateConfigurationSetReputationMetricsEnabledResponse =
  UpdateConfigurationSetReputationMetricsEnabledResponse'


instance NFData
           UpdateConfigurationSetReputationMetricsEnabledResponse
         where
