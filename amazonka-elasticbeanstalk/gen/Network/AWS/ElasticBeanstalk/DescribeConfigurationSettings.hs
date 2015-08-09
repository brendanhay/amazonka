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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the settings for the specified configuration
-- set, that is, either a configuration template or the configuration set
-- associated with a running environment.
--
-- When describing the settings for the configuration set associated with a
-- running environment, it is possible to receive two sets of setting
-- descriptions. One is the deployed configuration set, and the other is a
-- draft configuration of an environment that is either in the process of
-- deployment or that failed to deploy.
--
-- Related Topics
--
-- -   DeleteEnvironmentConfiguration
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeConfigurationSettings.html AWS API Reference> for DescribeConfigurationSettings.
module Network.AWS.ElasticBeanstalk.DescribeConfigurationSettings
    (
    -- * Creating a Request
      DescribeConfigurationSettings
    , describeConfigurationSettings
    -- * Request Lenses
    , dcsTemplateName
    , dcsEnvironmentName
    , dcsApplicationName

    -- * Destructuring the Response
    , DescribeConfigurationSettingsResponse
    , describeConfigurationSettingsResponse
    -- * Response Lenses
    , dcsrsConfigurationSettings
    , dcsrsStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Result message containing all of the configuration settings for a
-- specified solution stack or configuration template.
--
-- /See:/ 'describeConfigurationSettings' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsTemplateName'
--
-- * 'dcsEnvironmentName'
--
-- * 'dcsApplicationName'
data DescribeConfigurationSettings = DescribeConfigurationSettings'
    { _dcsTemplateName :: !(Maybe Text)
    , _dcsEnvironmentName :: !(Maybe Text)
    , _dcsApplicationName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConfigurationSettings' smart constructor.
describeConfigurationSettings :: Text -> DescribeConfigurationSettings
describeConfigurationSettings pApplicationName_ = 
    DescribeConfigurationSettings'
    { _dcsTemplateName = Nothing
    , _dcsEnvironmentName = Nothing
    , _dcsApplicationName = pApplicationName_
    }

-- | The name of the configuration template to describe.
--
-- Conditional: You must specify either this parameter or an
-- EnvironmentName, but not both. If you specify both, AWS Elastic
-- Beanstalk returns an @InvalidParameterCombination@ error. If you do not
-- specify either, AWS Elastic Beanstalk returns a
-- @MissingRequiredParameter@ error.
dcsTemplateName :: Lens' DescribeConfigurationSettings (Maybe Text)
dcsTemplateName = lens _dcsTemplateName (\ s a -> s{_dcsTemplateName = a});

-- | The name of the environment to describe.
--
-- Condition: You must specify either this or a TemplateName, but not both.
-- If you specify both, AWS Elastic Beanstalk returns an
-- @InvalidParameterCombination@ error. If you do not specify either, AWS
-- Elastic Beanstalk returns @MissingRequiredParameter@ error.
dcsEnvironmentName :: Lens' DescribeConfigurationSettings (Maybe Text)
dcsEnvironmentName = lens _dcsEnvironmentName (\ s a -> s{_dcsEnvironmentName = a});

-- | The application for the environment or configuration template.
dcsApplicationName :: Lens' DescribeConfigurationSettings Text
dcsApplicationName = lens _dcsApplicationName (\ s a -> s{_dcsApplicationName = a});

instance AWSRequest DescribeConfigurationSettings
         where
        type Sv DescribeConfigurationSettings =
             ElasticBeanstalk
        type Rs DescribeConfigurationSettings =
             DescribeConfigurationSettingsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeConfigurationSettingsResult"
              (\ s h x ->
                 DescribeConfigurationSettingsResponse' <$>
                   (x .@? "ConfigurationSettings" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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

-- | The results from a request to change the configuration settings of an
-- environment.
--
-- /See:/ 'describeConfigurationSettingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsrsConfigurationSettings'
--
-- * 'dcsrsStatus'
data DescribeConfigurationSettingsResponse = DescribeConfigurationSettingsResponse'
    { _dcsrsConfigurationSettings :: !(Maybe [ConfigurationSettingsDescription])
    , _dcsrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeConfigurationSettingsResponse' smart constructor.
describeConfigurationSettingsResponse :: Int -> DescribeConfigurationSettingsResponse
describeConfigurationSettingsResponse pStatus_ = 
    DescribeConfigurationSettingsResponse'
    { _dcsrsConfigurationSettings = Nothing
    , _dcsrsStatus = pStatus_
    }

-- | A list of ConfigurationSettingsDescription.
dcsrsConfigurationSettings :: Lens' DescribeConfigurationSettingsResponse [ConfigurationSettingsDescription]
dcsrsConfigurationSettings = lens _dcsrsConfigurationSettings (\ s a -> s{_dcsrsConfigurationSettings = a}) . _Default . _Coerce;

-- | Undocumented member.
dcsrsStatus :: Lens' DescribeConfigurationSettingsResponse Int
dcsrsStatus = lens _dcsrsStatus (\ s a -> s{_dcsrsStatus = a});
