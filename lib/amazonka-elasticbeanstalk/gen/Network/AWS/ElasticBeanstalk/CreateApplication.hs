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
-- Module      : Network.AWS.ElasticBeanstalk.CreateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application that has one configuration template named @default@ and no application versions.
--
--
module Network.AWS.ElasticBeanstalk.CreateApplication
    (
    -- * Creating a Request
      createApplication
    , CreateApplication
    -- * Request Lenses
    , caResourceLifecycleConfig
    , caDescription
    , caApplicationName

    -- * Destructuring the Response
    , applicationDescriptionMessage
    , ApplicationDescriptionMessage
    -- * Response Lenses
    , admApplication
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to create an application.
--
--
--
-- /See:/ 'createApplication' smart constructor.
data CreateApplication = CreateApplication'
  { _caResourceLifecycleConfig :: !(Maybe ApplicationResourceLifecycleConfig)
  , _caDescription             :: !(Maybe Text)
  , _caApplicationName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caResourceLifecycleConfig' - Specify an application resource lifecycle configuration to prevent your application from accumulating too many versions.
--
-- * 'caDescription' - Describes the application.
--
-- * 'caApplicationName' - The name of the application. Constraint: This name must be unique within your account. If the specified name already exists, the action returns an @InvalidParameterValue@ error.
createApplication
    :: Text -- ^ 'caApplicationName'
    -> CreateApplication
createApplication pApplicationName_ =
  CreateApplication'
    { _caResourceLifecycleConfig = Nothing
    , _caDescription = Nothing
    , _caApplicationName = pApplicationName_
    }


-- | Specify an application resource lifecycle configuration to prevent your application from accumulating too many versions.
caResourceLifecycleConfig :: Lens' CreateApplication (Maybe ApplicationResourceLifecycleConfig)
caResourceLifecycleConfig = lens _caResourceLifecycleConfig (\ s a -> s{_caResourceLifecycleConfig = a})

-- | Describes the application.
caDescription :: Lens' CreateApplication (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | The name of the application. Constraint: This name must be unique within your account. If the specified name already exists, the action returns an @InvalidParameterValue@ error.
caApplicationName :: Lens' CreateApplication Text
caApplicationName = lens _caApplicationName (\ s a -> s{_caApplicationName = a})

instance AWSRequest CreateApplication where
        type Rs CreateApplication =
             ApplicationDescriptionMessage
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "CreateApplicationResult"
              (\ s h x -> parseXML x)

instance Hashable CreateApplication where

instance NFData CreateApplication where

instance ToHeaders CreateApplication where
        toHeaders = const mempty

instance ToPath CreateApplication where
        toPath = const "/"

instance ToQuery CreateApplication where
        toQuery CreateApplication'{..}
          = mconcat
              ["Action" =: ("CreateApplication" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ResourceLifecycleConfig" =:
                 _caResourceLifecycleConfig,
               "Description" =: _caDescription,
               "ApplicationName" =: _caApplicationName]
