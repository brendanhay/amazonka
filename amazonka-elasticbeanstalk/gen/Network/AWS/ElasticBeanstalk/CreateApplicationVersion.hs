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
    , cavrqSourceBundle
    , cavrqAutoCreateApplication
    , cavrqDescription
    , cavrqApplicationName
    , cavrqVersionLabel

    -- * Response
    , ApplicationVersionDescriptionMessage
    -- ** Response constructor
    , applicationVersionDescriptionMessage
    -- ** Response lenses
    , cavrsApplicationVersion
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
-- * 'cavrqSourceBundle'
--
-- * 'cavrqAutoCreateApplication'
--
-- * 'cavrqDescription'
--
-- * 'cavrqApplicationName'
--
-- * 'cavrqVersionLabel'
data CreateApplicationVersion = CreateApplicationVersion'
    { _cavrqSourceBundle          :: !(Maybe S3Location)
    , _cavrqAutoCreateApplication :: !(Maybe Bool)
    , _cavrqDescription           :: !(Maybe Text)
    , _cavrqApplicationName       :: !Text
    , _cavrqVersionLabel          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateApplicationVersion' smart constructor.
createApplicationVersion :: Text -> Text -> CreateApplicationVersion
createApplicationVersion pApplicationName pVersionLabel =
    CreateApplicationVersion'
    { _cavrqSourceBundle = Nothing
    , _cavrqAutoCreateApplication = Nothing
    , _cavrqDescription = Nothing
    , _cavrqApplicationName = pApplicationName
    , _cavrqVersionLabel = pVersionLabel
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
cavrqSourceBundle :: Lens' CreateApplicationVersion (Maybe S3Location)
cavrqSourceBundle = lens _cavrqSourceBundle (\ s a -> s{_cavrqSourceBundle = a});

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
cavrqAutoCreateApplication :: Lens' CreateApplicationVersion (Maybe Bool)
cavrqAutoCreateApplication = lens _cavrqAutoCreateApplication (\ s a -> s{_cavrqAutoCreateApplication = a});

-- | Describes this version.
cavrqDescription :: Lens' CreateApplicationVersion (Maybe Text)
cavrqDescription = lens _cavrqDescription (\ s a -> s{_cavrqDescription = a});

-- | The name of the application. If no application is found with this name,
-- and @AutoCreateApplication@ is @false@, returns an
-- @InvalidParameterValue@ error.
cavrqApplicationName :: Lens' CreateApplicationVersion Text
cavrqApplicationName = lens _cavrqApplicationName (\ s a -> s{_cavrqApplicationName = a});

-- | A label identifying this version.
--
-- Constraint: Must be unique per application. If an application version
-- already exists with this label for the specified application, AWS
-- Elastic Beanstalk returns an @InvalidParameterValue@ error.
cavrqVersionLabel :: Lens' CreateApplicationVersion Text
cavrqVersionLabel = lens _cavrqVersionLabel (\ s a -> s{_cavrqVersionLabel = a});

instance AWSRequest CreateApplicationVersion where
        type Sv CreateApplicationVersion = ElasticBeanstalk
        type Rs CreateApplicationVersion =
             ApplicationVersionDescriptionMessage
        request = post
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
               "SourceBundle" =: _cavrqSourceBundle,
               "AutoCreateApplication" =:
                 _cavrqAutoCreateApplication,
               "Description" =: _cavrqDescription,
               "ApplicationName" =: _cavrqApplicationName,
               "VersionLabel" =: _cavrqVersionLabel]
