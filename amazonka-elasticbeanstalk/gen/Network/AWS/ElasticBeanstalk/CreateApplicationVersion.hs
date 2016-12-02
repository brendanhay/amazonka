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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version for the specified application.
--
--
module Network.AWS.ElasticBeanstalk.CreateApplicationVersion
    (
    -- * Creating a Request
      createApplicationVersion
    , CreateApplicationVersion
    -- * Request Lenses
    , cavProcess
    , cavSourceBundle
    , cavAutoCreateApplication
    , cavSourceBuildInformation
    , cavDescription
    , cavBuildConfiguration
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
--
--
-- /See:/ 'createApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
    { _cavProcess                :: !(Maybe Bool)
    , _cavSourceBundle           :: !(Maybe S3Location)
    , _cavAutoCreateApplication  :: !(Maybe Bool)
    , _cavSourceBuildInformation :: !(Maybe SourceBuildInformation)
    , _cavDescription            :: !(Maybe Text)
    , _cavBuildConfiguration     :: !(Maybe BuildConfiguration)
    , _cavApplicationName        :: !Text
    , _cavVersionLabel           :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateApplicationVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cavProcess' - Preprocesses and validates the environment manifest and configuration files in the source bundle. Validating configuration files can identify issues prior to deploying the application version to an environment.
--
-- * 'cavSourceBundle' - The Amazon S3 bucket and key that identify the location of the source bundle for this version. Specify a source bundle in S3 or a commit in an AWS CodeCommit repository (with @SourceBuildInformation@ ), but not both. If neither @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic Beanstalk uses a sample application.
--
-- * 'cavAutoCreateApplication' - Set to @true@ to create an application with the specified name if it doesn't already exist.
--
-- * 'cavSourceBuildInformation' - Specify a commit in an AWS CodeCommit Git repository to use as the source code for the application version. Specify a commit in an AWS CodeCommit repository or a source bundle in S3 (with @SourceBundle@ ), but not both. If neither @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic Beanstalk uses a sample application.
--
-- * 'cavDescription' - Describes this version.
--
-- * 'cavBuildConfiguration' - Undocumented member.
--
-- * 'cavApplicationName' - The name of the application. If no application is found with this name, and @AutoCreateApplication@ is @false@ , returns an @InvalidParameterValue@ error.
--
-- * 'cavVersionLabel' - A label identifying this version. Constraint: Must be unique per application. If an application version already exists with this label for the specified application, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
createApplicationVersion
    :: Text -- ^ 'cavApplicationName'
    -> Text -- ^ 'cavVersionLabel'
    -> CreateApplicationVersion
createApplicationVersion pApplicationName_ pVersionLabel_ =
    CreateApplicationVersion'
    { _cavProcess = Nothing
    , _cavSourceBundle = Nothing
    , _cavAutoCreateApplication = Nothing
    , _cavSourceBuildInformation = Nothing
    , _cavDescription = Nothing
    , _cavBuildConfiguration = Nothing
    , _cavApplicationName = pApplicationName_
    , _cavVersionLabel = pVersionLabel_
    }

-- | Preprocesses and validates the environment manifest and configuration files in the source bundle. Validating configuration files can identify issues prior to deploying the application version to an environment.
cavProcess :: Lens' CreateApplicationVersion (Maybe Bool)
cavProcess = lens _cavProcess (\ s a -> s{_cavProcess = a});

-- | The Amazon S3 bucket and key that identify the location of the source bundle for this version. Specify a source bundle in S3 or a commit in an AWS CodeCommit repository (with @SourceBuildInformation@ ), but not both. If neither @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic Beanstalk uses a sample application.
cavSourceBundle :: Lens' CreateApplicationVersion (Maybe S3Location)
cavSourceBundle = lens _cavSourceBundle (\ s a -> s{_cavSourceBundle = a});

-- | Set to @true@ to create an application with the specified name if it doesn't already exist.
cavAutoCreateApplication :: Lens' CreateApplicationVersion (Maybe Bool)
cavAutoCreateApplication = lens _cavAutoCreateApplication (\ s a -> s{_cavAutoCreateApplication = a});

-- | Specify a commit in an AWS CodeCommit Git repository to use as the source code for the application version. Specify a commit in an AWS CodeCommit repository or a source bundle in S3 (with @SourceBundle@ ), but not both. If neither @SourceBundle@ nor @SourceBuildInformation@ are provided, Elastic Beanstalk uses a sample application.
cavSourceBuildInformation :: Lens' CreateApplicationVersion (Maybe SourceBuildInformation)
cavSourceBuildInformation = lens _cavSourceBuildInformation (\ s a -> s{_cavSourceBuildInformation = a});

-- | Describes this version.
cavDescription :: Lens' CreateApplicationVersion (Maybe Text)
cavDescription = lens _cavDescription (\ s a -> s{_cavDescription = a});

-- | Undocumented member.
cavBuildConfiguration :: Lens' CreateApplicationVersion (Maybe BuildConfiguration)
cavBuildConfiguration = lens _cavBuildConfiguration (\ s a -> s{_cavBuildConfiguration = a});

-- | The name of the application. If no application is found with this name, and @AutoCreateApplication@ is @false@ , returns an @InvalidParameterValue@ error.
cavApplicationName :: Lens' CreateApplicationVersion Text
cavApplicationName = lens _cavApplicationName (\ s a -> s{_cavApplicationName = a});

-- | A label identifying this version. Constraint: Must be unique per application. If an application version already exists with this label for the specified application, AWS Elastic Beanstalk returns an @InvalidParameterValue@ error.
cavVersionLabel :: Lens' CreateApplicationVersion Text
cavVersionLabel = lens _cavVersionLabel (\ s a -> s{_cavVersionLabel = a});

instance AWSRequest CreateApplicationVersion where
        type Rs CreateApplicationVersion =
             ApplicationVersionDescriptionMessage
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "CreateApplicationVersionResult"
              (\ s h x -> parseXML x)

instance Hashable CreateApplicationVersion

instance NFData CreateApplicationVersion

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
               "Process" =: _cavProcess,
               "SourceBundle" =: _cavSourceBundle,
               "AutoCreateApplication" =: _cavAutoCreateApplication,
               "SourceBuildInformation" =:
                 _cavSourceBuildInformation,
               "Description" =: _cavDescription,
               "BuildConfiguration" =: _cavBuildConfiguration,
               "ApplicationName" =: _cavApplicationName,
               "VersionLabel" =: _cavVersionLabel]
