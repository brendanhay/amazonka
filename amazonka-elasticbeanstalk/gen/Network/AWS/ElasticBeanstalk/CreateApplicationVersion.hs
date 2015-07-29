{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateApplicationVersion
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version for the specified application.
--
-- Once you create an application version with a specified Amazon S3 bucket
-- and key location, you cannot change that Amazon S3 location. If you
-- change the Amazon S3 location, you receive an exception when you attempt
-- to launch an environment from the application version.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CreateApplicationVersion.html>
module Network.AWS.ElasticBeanstalk.CreateApplicationVersion
    (
    -- * Request
      CreateApplicationVersion
    -- ** Request constructor
    , createApplicationVersion
    -- ** Request lenses
    , cavSourceBundle
    , cavAutoCreateApplication
    , cavDescription
    , cavApplicationName
    , cavVersionLabel

    -- * Response
    , ApplicationVersionDescriptionMessage
    -- ** Response constructor
    , applicationVersionDescriptionMessage
    -- ** Response lenses
    , avdmApplicationVersion
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createApplicationVersion' smart constructor.
--
-- The fields accessible through corresponding lenses are:
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
data CreateApplicationVersion = CreateApplicationVersion'
    { _cavSourceBundle          :: !(Maybe S3Location)
    , _cavAutoCreateApplication :: !(Maybe Bool)
    , _cavDescription           :: !(Maybe Text)
    , _cavApplicationName       :: !Text
    , _cavVersionLabel          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateApplicationVersion' smart constructor.
createApplicationVersion :: Text -> Text -> CreateApplicationVersion
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
-- @InvalidParameterValue@ error. The maximum size allowed is 512 MB.
--
-- Default: If not specified, AWS Elastic Beanstalk uses a sample
-- application. If only partially specified (for example, a bucket is
-- provided but not the key) or if no data is found at the Amazon S3
-- location, AWS Elastic Beanstalk returns an @InvalidParameterCombination@
-- error.
cavSourceBundle :: Lens' CreateApplicationVersion (Maybe S3Location)
cavSourceBundle = lens _cavSourceBundle (\ s a -> s{_cavSourceBundle = a});

-- | Determines how the system behaves if the specified application for this
-- version does not already exist:
--
-- @true@: Automatically creates the specified application for this version
-- if it does not already exist.
--
-- @false@: Returns an @InvalidParameterValue@ if the specified application
-- for this version does not already exist.
--
-- -   @true@ : Automatically creates the specified application for this
--     release if it does not already exist.
-- -   @false@ : Throws an @InvalidParameterValue@ if the specified
--     application for this release does not already exist.
--
-- Default: @false@
--
-- Valid Values: @true@ | @false@
cavAutoCreateApplication :: Lens' CreateApplicationVersion (Maybe Bool)
cavAutoCreateApplication = lens _cavAutoCreateApplication (\ s a -> s{_cavAutoCreateApplication = a});

-- | Describes this version.
cavDescription :: Lens' CreateApplicationVersion (Maybe Text)
cavDescription = lens _cavDescription (\ s a -> s{_cavDescription = a});

-- | The name of the application. If no application is found with this name,
-- and @AutoCreateApplication@ is @false@, returns an
-- @InvalidParameterValue@ error.
cavApplicationName :: Lens' CreateApplicationVersion Text
cavApplicationName = lens _cavApplicationName (\ s a -> s{_cavApplicationName = a});

-- | A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version
-- already exists with this label for the specified application, AWS
-- Elastic Beanstalk returns an @InvalidParameterValue@ error.
cavVersionLabel :: Lens' CreateApplicationVersion Text
cavVersionLabel = lens _cavVersionLabel (\ s a -> s{_cavVersionLabel = a});

instance AWSRequest CreateApplicationVersion where
        type Sv CreateApplicationVersion = ElasticBeanstalk
        type Rs CreateApplicationVersion =
             ApplicationVersionDescriptionMessage
        request = postQuery
        response
          = receiveXMLWrapper "CreateApplicationVersionResult"
              (\ s h x -> parseXML x)

instance ToHeaders CreateApplicationVersion where
        toHeaders = const mempty

instance ToPath CreateApplicationVersion where
        toPath = const mempty

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
