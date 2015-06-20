{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the configuration options that are used in a particular
-- configuration template or environment, or that a specified solution
-- stack defines. The description includes the values the options, their
-- default values, and an indication of the required action on a running
-- environment if an option value is changed.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeConfigurationOptions.html>
module Network.AWS.ElasticBeanstalk.DescribeConfigurationOptions
    (
    -- * Request
      DescribeConfigurationOptions
    -- ** Request constructor
    , describeConfigurationOptions
    -- ** Request lenses
    , dcoTemplateName
    , dcoEnvironmentName
    , dcoApplicationName
    , dcoOptions
    , dcoSolutionStackName

    -- * Response
    , DescribeConfigurationOptionsResponse
    -- ** Response constructor
    , describeConfigurationOptionsResponse
    -- ** Response lenses
    , dcorOptions
    , dcorSolutionStackName
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConfigurationOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data DescribeConfigurationOptions = DescribeConfigurationOptions'{_dcoTemplateName :: Maybe Text, _dcoEnvironmentName :: Maybe Text, _dcoApplicationName :: Maybe Text, _dcoOptions :: Maybe [OptionSpecification], _dcoSolutionStackName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeConfigurationOptions' smart constructor.
describeConfigurationOptions :: DescribeConfigurationOptions
describeConfigurationOptions = DescribeConfigurationOptions'{_dcoTemplateName = Nothing, _dcoEnvironmentName = Nothing, _dcoApplicationName = Nothing, _dcoOptions = Nothing, _dcoSolutionStackName = Nothing};

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
dcoOptions = lens _dcoOptions (\ s a -> s{_dcoOptions = a}) . _Default;

-- | The name of the solution stack whose configuration options you want to
-- describe.
dcoSolutionStackName :: Lens' DescribeConfigurationOptions (Maybe Text)
dcoSolutionStackName = lens _dcoSolutionStackName (\ s a -> s{_dcoSolutionStackName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DescribeConfigurationOptions
         where
        type Sv DescribeConfigurationOptions =
             ElasticBeanstalk
        type Rs DescribeConfigurationOptions =
             DescribeConfigurationOptionsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeConfigurationOptionsResult"
              (\ s h x ->
                 DescribeConfigurationOptionsResponse' <$>
                   (x .@? "Options" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "SolutionStackName"))

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

-- | /See:/ 'describeConfigurationOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcorOptions'
--
-- * 'dcorSolutionStackName'
data DescribeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'{_dcorOptions :: Maybe [ConfigurationOptionDescription], _dcorSolutionStackName :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeConfigurationOptionsResponse' smart constructor.
describeConfigurationOptionsResponse :: DescribeConfigurationOptionsResponse
describeConfigurationOptionsResponse = DescribeConfigurationOptionsResponse'{_dcorOptions = Nothing, _dcorSolutionStackName = Nothing};

-- | A list of ConfigurationOptionDescription.
dcorOptions :: Lens' DescribeConfigurationOptionsResponse [ConfigurationOptionDescription]
dcorOptions = lens _dcorOptions (\ s a -> s{_dcorOptions = a}) . _Default;

-- | The name of the solution stack these configuration options belong to.
dcorSolutionStackName :: Lens' DescribeConfigurationOptionsResponse (Maybe Text)
dcorSolutionStackName = lens _dcorSolutionStackName (\ s a -> s{_dcorSolutionStackName = a});
