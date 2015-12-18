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
-- Module      : Network.AWS.ElasticBeanstalk.CreateApplicationVersion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version for the specified application.
--
-- Once you create an application version with a specified Amazon S3 bucket
-- and key location, you cannot change that Amazon S3 location. If you
-- change the Amazon S3 location, you receive an exception when you attempt
-- to launch an environment from the application version.
--
-- /See:/ <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CreateApplicationVersion.html AWS API Reference> for CreateApplicationVersion.
module Network.AWS.ElasticBeanstalk.CreateApplicationVersion
    (
    -- * Creating a Request
      createApplicationVersion
    , CreateApplicationVersion
    -- * Request Lenses
    , cavSourceBundle
    , cavAutoCreateApplication
    , cavDescription
    , cavApplicationName
    , cavVersionLabel

    -- * Destructuring the Response
    , applicationVersionDescriptionMessage
    , ApplicationVersionDescriptionMessage
    -- * Response Lenses
    , avdmApplicationVersion
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.ElasticBeanstalk.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
    { _cavSourceBundle          :: !(Maybe S3Location)
    , _cavAutoCreateApplication :: !(Maybe Bool)
    , _cavDescription           :: !(Maybe Text)
    , _cavApplicationName       :: !Text
    , _cavVersionLabel          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateApplicationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cavSourceBundle'
--
-- * 'cavAutoCreateApplication'
--
-- * 'cavDescription'
--
-- * 'cavApplicationName'
--
-- * 'cavVersionLabel'
createApplicationVersion
    :: Text -- ^ 'cavApplicationName'
    -> Text -- ^ 'cavVersionLabel'
    -> CreateApplicationVersion
createApplicationVersion pApplicationName_ pVersionLabel_ =
    CreateApplicationVersion'
    { _cavSourceBundle = Nothing
    , _cavAutoCreateApplication = Nothing
    , _cavDescription = Nothing
    , _cavApplicationName = pApplicationName_
    , _cavVersionLabel = pVersionLabel_
    }

-- | The Amazon S3 bucket and key that identify the location of the source
-- bundle for this version.
--
-- If data found at the Amazon S3 location exceeds the maximum allowed
-- source bundle size, AWS Elastic Beanstalk returns an
-- 'InvalidParameterValue' error. The maximum size allowed is 512 MB.
--
-- Default: If not specified, AWS Elastic Beanstalk uses a sample
-- application. If only partially specified (for example, a bucket is
-- provided but not the key) or if no data is found at the Amazon S3
-- location, AWS Elastic Beanstalk returns an 'InvalidParameterCombination'
-- error.
cavSourceBundle :: Lens' CreateApplicationVersion (Maybe S3Location)
cavSourceBundle = lens _cavSourceBundle (\ s a -> s{_cavSourceBundle = a});

-- | Determines how the system behaves if the specified application for this
-- version does not already exist:
--
-- 'true': Automatically creates the specified application for this version
-- if it does not already exist.
--
-- 'false': Returns an 'InvalidParameterValue' if the specified application
-- for this version does not already exist.
--
-- -   'true' : Automatically creates the specified application for this
--     release if it does not already exist.
-- -   'false' : Throws an 'InvalidParameterValue' if the specified
--     application for this release does not already exist.
--
-- Default: 'false'
--
-- Valid Values: 'true' | 'false'
cavAutoCreateApplication :: Lens' CreateApplicationVersion (Maybe Bool)
cavAutoCreateApplication = lens _cavAutoCreateApplication (\ s a -> s{_cavAutoCreateApplication = a});

-- | Describes this version.
cavDescription :: Lens' CreateApplicationVersion (Maybe Text)
cavDescription = lens _cavDescription (\ s a -> s{_cavDescription = a});

-- | The name of the application. If no application is found with this name,
-- and 'AutoCreateApplication' is 'false', returns an
-- 'InvalidParameterValue' error.
cavApplicationName :: Lens' CreateApplicationVersion Text
cavApplicationName = lens _cavApplicationName (\ s a -> s{_cavApplicationName = a});

-- | A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version
-- already exists with this label for the specified application, AWS
-- Elastic Beanstalk returns an 'InvalidParameterValue' error.
cavVersionLabel :: Lens' CreateApplicationVersion Text
cavVersionLabel = lens _cavVersionLabel (\ s a -> s{_cavVersionLabel = a});

instance AWSRequest CreateApplicationVersion where
        type Rs CreateApplicationVersion =
             ApplicationVersionDescriptionMessage
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "CreateApplicationVersionResult"
              (\ s h x -> parseXML x)

instance ToHeaders CreateApplicationVersion where
        toHeaders = const mempty

instance ToPath CreateApplicationVersion where
        toPath = const "/"

instance ToQuery CreateApplicationVersion where
        toQuery CreateApplicationVersion'{..}
          = mconcat
              ["Action" =:
                 ("CreateApplicationVersion" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "SourceBundle" =: _cavSourceBundle,
               "AutoCreateApplication" =: _cavAutoCreateApplication,
               "Description" =: _cavDescription,
               "ApplicationName" =: _cavApplicationName,
               "VersionLabel" =: _cavVersionLabel]
