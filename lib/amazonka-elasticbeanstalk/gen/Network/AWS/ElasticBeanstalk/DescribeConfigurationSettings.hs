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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the settings for the specified configuration set, that is, either a configuration template or the configuration set associated with a running environment.
--
--
-- When describing the settings for the configuration set associated with a running environment, it is possible to receive two sets of setting descriptions. One is the deployed configuration set, and the other is a draft configuration of an environment that is either in the process of deployment or that failed to deploy.
--
-- Related Topics
--
--     * 'DeleteEnvironmentConfiguration'
--
--
--
module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
    (
    -- * Creating a Request
      describeConfigurationSettings
    , DescribeConfigurationSettings
    -- * Request Lenses
    , dcsTemplateName
    , dcsEnvironmentName
    , dcsApplicationName

    -- * Destructuring the Response
    , describeConfigurationSettingsResponse
    , DescribeConfigurationSettingsResponse
    -- * Response Lenses
    , dcsrsConfigurationSettings
    , dcsrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Result message containing all of the configuration settings for a specified solution stack or configuration template.
--
--
--
-- /See:/ 'describeConfigurationSettings' smart constructor.
data DescribeConfigurationSettings = DescribeConfigurationSettings'
  { _dcsTemplateName    :: !(Maybe Text)
  , _dcsEnvironmentName :: !(Maybe Text)
  , _dcsApplicationName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsTemplateName' - The name of the configuration template to describe. Conditional: You must specify either this parameter or an EnvironmentName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns a @MissingRequiredParameter@ error.
--
-- * 'dcsEnvironmentName' - The name of the environment to describe. Condition: You must specify either this or a TemplateName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- * 'dcsApplicationName' - The application for the environment or configuration template.
describeConfigurationSettings
    :: Text -- ^ 'dcsApplicationName'
    -> DescribeConfigurationSettings
describeConfigurationSettings pApplicationName_ =
  DescribeConfigurationSettings'
    { _dcsTemplateName = Nothing
    , _dcsEnvironmentName = Nothing
    , _dcsApplicationName = pApplicationName_
    }


-- | The name of the configuration template to describe. Conditional: You must specify either this parameter or an EnvironmentName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns a @MissingRequiredParameter@ error.
dcsTemplateName :: Lens' DescribeConfigurationSettings (Maybe Text)
dcsTemplateName = lens _dcsTemplateName (\ s a -> s{_dcsTemplateName = a})

-- | The name of the environment to describe. Condition: You must specify either this or a TemplateName, but not both. If you specify both, AWS Elastic Beanstalk returns an @InvalidParameterCombination@ error. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
dcsEnvironmentName :: Lens' DescribeConfigurationSettings (Maybe Text)
dcsEnvironmentName = lens _dcsEnvironmentName (\ s a -> s{_dcsEnvironmentName = a})

-- | The application for the environment or configuration template.
dcsApplicationName :: Lens' DescribeConfigurationSettings Text
dcsApplicationName = lens _dcsApplicationName (\ s a -> s{_dcsApplicationName = a})

instance AWSRequest DescribeConfigurationSettings
         where
        type Rs DescribeConfigurationSettings =
             DescribeConfigurationSettingsResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "DescribeConfigurationSettingsResult"
              (\ s h x ->
                 DescribeConfigurationSettingsResponse' <$>
                   (x .@? "ConfigurationSettings" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConfigurationSettings where

instance NFData DescribeConfigurationSettings where

instance ToHeaders DescribeConfigurationSettings
         where
        toHeaders = const mempty

instance ToPath DescribeConfigurationSettings where
        toPath = const "/"

instance ToQuery DescribeConfigurationSettings where
        toQuery DescribeConfigurationSettings'{..}
          = mconcat
              ["Action" =:
                 ("DescribeConfigurationSettings" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _dcsTemplateName,
               "EnvironmentName" =: _dcsEnvironmentName,
               "ApplicationName" =: _dcsApplicationName]

-- | The results from a request to change the configuration settings of an environment.
--
--
--
-- /See:/ 'describeConfigurationSettingsResponse' smart constructor.
data DescribeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse'
  { _dcsrsConfigurationSettings :: !(Maybe [ConfigurationSettingsDescription])
  , _dcsrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsrsConfigurationSettings' - A list of 'ConfigurationSettingsDescription' .
--
-- * 'dcsrsResponseStatus' - -- | The response status code.
describeConfigurationSettingsResponse
    :: Int -- ^ 'dcsrsResponseStatus'
    -> DescribeConfigurationSettingsResponse
describeConfigurationSettingsResponse pResponseStatus_ =
  DescribeConfigurationSettingsResponse'
    { _dcsrsConfigurationSettings = Nothing
    , _dcsrsResponseStatus = pResponseStatus_
    }


-- | A list of 'ConfigurationSettingsDescription' .
dcsrsConfigurationSettings :: Lens' DescribeConfigurationSettingsResponse [ConfigurationSettingsDescription]
dcsrsConfigurationSettings = lens _dcsrsConfigurationSettings (\ s a -> s{_dcsrsConfigurationSettings = a}) . _Default . _Coerce

-- | -- | The response status code.
dcsrsResponseStatus :: Lens' DescribeConfigurationSettingsResponse Int
dcsrsResponseStatus = lens _dcsrsResponseStatus (\ s a -> s{_dcsrsResponseStatus = a})

instance NFData DescribeConfigurationSettingsResponse
         where
