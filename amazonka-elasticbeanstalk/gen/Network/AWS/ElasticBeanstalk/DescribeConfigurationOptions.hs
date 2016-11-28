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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the configuration options that are used in a particular configuration template or environment, or that a specified solution stack defines. The description includes the values the options, their default values, and an indication of the required action on a running environment if an option value is changed.
--
--
module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
    (
    -- * Creating a Request
      describeConfigurationOptions
    , DescribeConfigurationOptions
    -- * Request Lenses
    , dcoTemplateName
    , dcoEnvironmentName
    , dcoApplicationName
    , dcoSolutionStackName
    , dcoOptions

    -- * Destructuring the Response
    , describeConfigurationOptionsResponse
    , DescribeConfigurationOptionsResponse
    -- * Response Lenses
    , dcorsSolutionStackName
    , dcorsOptions
    , dcorsResponseStatus
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Result message containig a list of application version descriptions.
--
--
--
-- /See:/ 'describeConfigurationOptions' smart constructor.
data DescribeConfigurationOptions = DescribeConfigurationOptions'
    { _dcoTemplateName      :: !(Maybe Text)
    , _dcoEnvironmentName   :: !(Maybe Text)
    , _dcoApplicationName   :: !(Maybe Text)
    , _dcoSolutionStackName :: !(Maybe Text)
    , _dcoOptions           :: !(Maybe [OptionSpecification])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConfigurationOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcoTemplateName' - The name of the configuration template whose configuration options you want to describe.
--
-- * 'dcoEnvironmentName' - The name of the environment whose configuration options you want to describe.
--
-- * 'dcoApplicationName' - The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
--
-- * 'dcoSolutionStackName' - The name of the solution stack whose configuration options you want to describe.
--
-- * 'dcoOptions' - If specified, restricts the descriptions to only the specified options.
describeConfigurationOptions
    :: DescribeConfigurationOptions
describeConfigurationOptions =
    DescribeConfigurationOptions'
    { _dcoTemplateName = Nothing
    , _dcoEnvironmentName = Nothing
    , _dcoApplicationName = Nothing
    , _dcoSolutionStackName = Nothing
    , _dcoOptions = Nothing
    }

-- | The name of the configuration template whose configuration options you want to describe.
dcoTemplateName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoTemplateName = lens _dcoTemplateName (\ s a -> s{_dcoTemplateName = a});

-- | The name of the environment whose configuration options you want to describe.
dcoEnvironmentName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoEnvironmentName = lens _dcoEnvironmentName (\ s a -> s{_dcoEnvironmentName = a});

-- | The name of the application associated with the configuration template or environment. Only needed if you want to describe the configuration options associated with either the configuration template or environment.
dcoApplicationName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoApplicationName = lens _dcoApplicationName (\ s a -> s{_dcoApplicationName = a});

-- | The name of the solution stack whose configuration options you want to describe.
dcoSolutionStackName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoSolutionStackName = lens _dcoSolutionStackName (\ s a -> s{_dcoSolutionStackName = a});

-- | If specified, restricts the descriptions to only the specified options.
dcoOptions :: Lens' DescribeConfigurationOptions [OptionSpecification]
dcoOptions = lens _dcoOptions (\ s a -> s{_dcoOptions = a}) . _Default . _Coerce;

instance AWSRequest DescribeConfigurationOptions
         where
        type Rs DescribeConfigurationOptions =
             DescribeConfigurationOptionsResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "DescribeConfigurationOptionsResult"
              (\ s h x ->
                 DescribeConfigurationOptionsResponse' <$>
                   (x .@? "SolutionStackName") <*>
                     (x .@? "Options" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConfigurationOptions

instance NFData DescribeConfigurationOptions

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
               "SolutionStackName" =: _dcoSolutionStackName,
               "Options" =:
                 toQuery (toQueryList "member" <$> _dcoOptions)]

-- | Describes the settings for a specified configuration set.
--
--
--
-- /See:/ 'describeConfigurationOptionsResponse' smart constructor.
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'
    { _dcorsSolutionStackName :: !(Maybe Text)
    , _dcorsOptions           :: !(Maybe [ConfigurationOptionDescription])
    , _dcorsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConfigurationOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcorsSolutionStackName' - The name of the solution stack these configuration options belong to.
--
-- * 'dcorsOptions' - A list of 'ConfigurationOptionDescription' .
--
-- * 'dcorsResponseStatus' - -- | The response status code.
describeConfigurationOptionsResponse
    :: Int -- ^ 'dcorsResponseStatus'
    -> DescribeConfigurationOptionsResponse
describeConfigurationOptionsResponse pResponseStatus_ =
    DescribeConfigurationOptionsResponse'
    { _dcorsSolutionStackName = Nothing
    , _dcorsOptions = Nothing
    , _dcorsResponseStatus = pResponseStatus_
    }

-- | The name of the solution stack these configuration options belong to.
dcorsSolutionStackName :: Lens' DescribeConfigurationOptionsResponse (Maybe Text)
dcorsSolutionStackName = lens _dcorsSolutionStackName (\ s a -> s{_dcorsSolutionStackName = a});

-- | A list of 'ConfigurationOptionDescription' .
dcorsOptions :: Lens' DescribeConfigurationOptionsResponse [ConfigurationOptionDescription]
dcorsOptions = lens _dcorsOptions (\ s a -> s{_dcorsOptions = a}) . _Default . _Coerce;

-- | -- | The response status code.
dcorsResponseStatus :: Lens' DescribeConfigurationOptionsResponse Int
dcorsResponseStatus = lens _dcorsResponseStatus (\ s a -> s{_dcorsResponseStatus = a});

instance NFData DescribeConfigurationOptionsResponse
