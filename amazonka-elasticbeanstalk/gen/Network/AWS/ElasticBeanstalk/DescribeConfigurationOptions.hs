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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the configuration options that are used in a particular
-- configuration template or environment, or that a specified solution
-- stack defines. The description includes the values the options, their
-- default values, and an indication of the required action on a running
-- environment if an option value is changed.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeConfigurationOptions.html AWS API Reference> for DescribeConfigurationOptions.
module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
    (
    -- * Creating a Request
      describeConfigurationOptions
    , DescribeConfigurationOptions
    -- * Request Lenses
    , dcoTemplateName
    , dcoEnvironmentName
    , dcoApplicationName
    , dcoOptions
    , dcoSolutionStackName

    -- * Destructuring the Response
    , describeConfigurationOptionsResponse
    , DescribeConfigurationOptionsResponse
    -- * Response Lenses
    , dcorsOptions
    , dcorsSolutionStackName
    , dcorsStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Result message containig a list of application version descriptions.
--
-- /See:/ 'describeConfigurationOptions' smart constructor.
data DescribeConfigurationOptions = DescribeConfigurationOptions'
    { _dcoTemplateName      :: !(Maybe Text)
    , _dcoEnvironmentName   :: !(Maybe Text)
    , _dcoApplicationName   :: !(Maybe Text)
    , _dcoOptions           :: !(Maybe [OptionSpecification])
    , _dcoSolutionStackName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConfigurationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcoTemplateName'
--
-- * 'dcoEnvironmentName'
--
-- * 'dcoApplicationName'
--
-- * 'dcoOptions'
--
-- * 'dcoSolutionStackName'
describeConfigurationOptions
    :: DescribeConfigurationOptions
describeConfigurationOptions =
    DescribeConfigurationOptions'
    { _dcoTemplateName = Nothing
    , _dcoEnvironmentName = Nothing
    , _dcoApplicationName = Nothing
    , _dcoOptions = Nothing
    , _dcoSolutionStackName = Nothing
    }

-- | The name of the configuration template whose configuration options you
-- want to describe.
dcoTemplateName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoTemplateName = lens _dcoTemplateName (\ s a -> s{_dcoTemplateName = a});

-- | The name of the environment whose configuration options you want to
-- describe.
dcoEnvironmentName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoEnvironmentName = lens _dcoEnvironmentName (\ s a -> s{_dcoEnvironmentName = a});

-- | The name of the application associated with the configuration template
-- or environment. Only needed if you want to describe the configuration
-- options associated with either the configuration template or
-- environment.
dcoApplicationName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoApplicationName = lens _dcoApplicationName (\ s a -> s{_dcoApplicationName = a});

-- | If specified, restricts the descriptions to only the specified options.
dcoOptions :: Lens' DescribeConfigurationOptions [OptionSpecification]
dcoOptions = lens _dcoOptions (\ s a -> s{_dcoOptions = a}) . _Default . _Coerce;

-- | The name of the solution stack whose configuration options you want to
-- describe.
dcoSolutionStackName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoSolutionStackName = lens _dcoSolutionStackName (\ s a -> s{_dcoSolutionStackName = a});

instance AWSRequest DescribeConfigurationOptions
         where
        type Sv DescribeConfigurationOptions =
             ElasticBeanstalk
        type Rs DescribeConfigurationOptions =
             DescribeConfigurationOptionsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeConfigurationOptionsResult"
              (\ s h x ->
                 DescribeConfigurationOptionsResponse' <$>
                   (x .@? "Options" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "SolutionStackName")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeConfigurationOptions where
        toHeaders = const mempty

instance ToPath DescribeConfigurationOptions where
        toPath = const "/"

instance ToQuery DescribeConfigurationOptions where
        toQuery DescribeConfigurationOptions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeConfigurationOptions" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "TemplateName" =: _dcoTemplateName,
               "EnvironmentName" =: _dcoEnvironmentName,
               "ApplicationName" =: _dcoApplicationName,
               "Options" =:
                 toQuery (toQueryList "member" <$> _dcoOptions),
               "SolutionStackName" =: _dcoSolutionStackName]

-- | Describes the settings for a specified configuration set.
--
-- /See:/ 'describeConfigurationOptionsResponse' smart constructor.
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'
    { _dcorsOptions           :: !(Maybe [ConfigurationOptionDescription])
    , _dcorsSolutionStackName :: !(Maybe Text)
    , _dcorsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConfigurationOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcorsOptions'
--
-- * 'dcorsSolutionStackName'
--
-- * 'dcorsStatus'
describeConfigurationOptionsResponse
    :: Int -- ^ 'dcorsStatus'
    -> DescribeConfigurationOptionsResponse
describeConfigurationOptionsResponse pStatus_ =
    DescribeConfigurationOptionsResponse'
    { _dcorsOptions = Nothing
    , _dcorsSolutionStackName = Nothing
    , _dcorsStatus = pStatus_
    }

-- | A list of ConfigurationOptionDescription.
dcorsOptions :: Lens' DescribeConfigurationOptionsResponse [ConfigurationOptionDescription]
dcorsOptions = lens _dcorsOptions (\ s a -> s{_dcorsOptions = a}) . _Default . _Coerce;

-- | The name of the solution stack these configuration options belong to.
dcorsSolutionStackName :: Lens' DescribeConfigurationOptionsResponse (Maybe Text)
dcorsSolutionStackName = lens _dcorsSolutionStackName (\ s a -> s{_dcorsSolutionStackName = a});

-- | The response status code.
dcorsStatus :: Lens' DescribeConfigurationOptionsResponse Int
dcorsStatus = lens _dcorsStatus (\ s a -> s{_dcorsStatus = a});
