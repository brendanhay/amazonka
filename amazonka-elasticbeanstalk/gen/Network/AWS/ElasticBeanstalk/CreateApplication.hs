{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.CreateApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an application that has one configuration template named
-- @default@ and no application versions.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_CreateApplication.html>
module Network.AWS.ElasticBeanstalk.CreateApplication
    (
    -- * Request
      CreateApplication
    -- ** Request constructor
    , createApplication
    -- ** Request lenses
    , carqDescription
    , carqApplicationName

    -- * Response
    , ApplicationDescriptionMessage
    -- ** Response constructor
    , applicationDescriptionMessage
    -- ** Response lenses
    , admApplication
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This documentation target is not reported in the API reference.
--
-- /See:/ 'createApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carqDescription'
--
-- * 'carqApplicationName'
data CreateApplication = CreateApplication'
    { _carqDescription     :: !(Maybe Text)
    , _carqApplicationName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateApplication' smart constructor.
createApplication :: Text -> CreateApplication
createApplication pApplicationName_ =
    CreateApplication'
    { _carqDescription = Nothing
    , _carqApplicationName = pApplicationName_
    }

-- | Describes the application.
carqDescription :: Lens' CreateApplication (Maybe Text)
carqDescription = lens _carqDescription (\ s a -> s{_carqDescription = a});

-- | The name of the application.
--
-- Constraint: This name must be unique within your account. If the
-- specified name already exists, the action returns an
-- @InvalidParameterValue@ error.
carqApplicationName :: Lens' CreateApplication Text
carqApplicationName = lens _carqApplicationName (\ s a -> s{_carqApplicationName = a});

instance AWSRequest CreateApplication where
        type Sv CreateApplication = ElasticBeanstalk
        type Rs CreateApplication =
             ApplicationDescriptionMessage
        request = post
        response
          = receiveXMLWrapper "CreateApplicationResult"
              (\ s h x -> parseXML x)

instance ToHeaders CreateApplication where
        toHeaders = const mempty

instance ToPath CreateApplication where
        toPath = const "/"

instance ToQuery CreateApplication where
        toQuery CreateApplication'{..}
          = mconcat
              ["Action" =: ("CreateApplication" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Description" =: _carqDescription,
               "ApplicationName" =: _carqApplicationName]
