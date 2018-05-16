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
-- Module      : Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies lifecycle settings for an application.
--
--
module Network.AWS.ElasticBeanstalk.UpdateApplicationResourceLifecycle
    (
    -- * Creating a Request
      updateApplicationResourceLifecycle
    , UpdateApplicationResourceLifecycle
    -- * Request Lenses
    , uarlApplicationName
    , uarlResourceLifecycleConfig

    -- * Destructuring the Response
    , updateApplicationResourceLifecycleResponse
    , UpdateApplicationResourceLifecycleResponse
    -- * Response Lenses
    , uarlrsApplicationName
    , uarlrsResourceLifecycleConfig
    , uarlrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateApplicationResourceLifecycle' smart constructor.
data UpdateApplicationResourceLifecycle = UpdateApplicationResourceLifecycle'
  { _uarlApplicationName         :: !Text
  , _uarlResourceLifecycleConfig :: !ApplicationResourceLifecycleConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationResourceLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarlApplicationName' - The name of the application.
--
-- * 'uarlResourceLifecycleConfig' - The lifecycle configuration.
updateApplicationResourceLifecycle
    :: Text -- ^ 'uarlApplicationName'
    -> ApplicationResourceLifecycleConfig -- ^ 'uarlResourceLifecycleConfig'
    -> UpdateApplicationResourceLifecycle
updateApplicationResourceLifecycle pApplicationName_ pResourceLifecycleConfig_ =
  UpdateApplicationResourceLifecycle'
    { _uarlApplicationName = pApplicationName_
    , _uarlResourceLifecycleConfig = pResourceLifecycleConfig_
    }


-- | The name of the application.
uarlApplicationName :: Lens' UpdateApplicationResourceLifecycle Text
uarlApplicationName = lens _uarlApplicationName (\ s a -> s{_uarlApplicationName = a})

-- | The lifecycle configuration.
uarlResourceLifecycleConfig :: Lens' UpdateApplicationResourceLifecycle ApplicationResourceLifecycleConfig
uarlResourceLifecycleConfig = lens _uarlResourceLifecycleConfig (\ s a -> s{_uarlResourceLifecycleConfig = a})

instance AWSRequest
           UpdateApplicationResourceLifecycle
         where
        type Rs UpdateApplicationResourceLifecycle =
             UpdateApplicationResourceLifecycleResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "UpdateApplicationResourceLifecycleResult"
              (\ s h x ->
                 UpdateApplicationResourceLifecycleResponse' <$>
                   (x .@? "ApplicationName") <*>
                     (x .@? "ResourceLifecycleConfig")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateApplicationResourceLifecycle
         where

instance NFData UpdateApplicationResourceLifecycle
         where

instance ToHeaders UpdateApplicationResourceLifecycle
         where
        toHeaders = const mempty

instance ToPath UpdateApplicationResourceLifecycle
         where
        toPath = const "/"

instance ToQuery UpdateApplicationResourceLifecycle
         where
        toQuery UpdateApplicationResourceLifecycle'{..}
          = mconcat
              ["Action" =:
                 ("UpdateApplicationResourceLifecycle" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "ApplicationName" =: _uarlApplicationName,
               "ResourceLifecycleConfig" =:
                 _uarlResourceLifecycleConfig]

-- | /See:/ 'updateApplicationResourceLifecycleResponse' smart constructor.
data UpdateApplicationResourceLifecycleResponse = UpdateApplicationResourceLifecycleResponse'
  { _uarlrsApplicationName :: !(Maybe Text)
  , _uarlrsResourceLifecycleConfig :: !(Maybe ApplicationResourceLifecycleConfig)
  , _uarlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationResourceLifecycleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarlrsApplicationName' - The name of the application.
--
-- * 'uarlrsResourceLifecycleConfig' - The lifecycle configuration.
--
-- * 'uarlrsResponseStatus' - -- | The response status code.
updateApplicationResourceLifecycleResponse
    :: Int -- ^ 'uarlrsResponseStatus'
    -> UpdateApplicationResourceLifecycleResponse
updateApplicationResourceLifecycleResponse pResponseStatus_ =
  UpdateApplicationResourceLifecycleResponse'
    { _uarlrsApplicationName = Nothing
    , _uarlrsResourceLifecycleConfig = Nothing
    , _uarlrsResponseStatus = pResponseStatus_
    }


-- | The name of the application.
uarlrsApplicationName :: Lens' UpdateApplicationResourceLifecycleResponse (Maybe Text)
uarlrsApplicationName = lens _uarlrsApplicationName (\ s a -> s{_uarlrsApplicationName = a})

-- | The lifecycle configuration.
uarlrsResourceLifecycleConfig :: Lens' UpdateApplicationResourceLifecycleResponse (Maybe ApplicationResourceLifecycleConfig)
uarlrsResourceLifecycleConfig = lens _uarlrsResourceLifecycleConfig (\ s a -> s{_uarlrsResourceLifecycleConfig = a})

-- | -- | The response status code.
uarlrsResponseStatus :: Lens' UpdateApplicationResourceLifecycleResponse Int
uarlrsResponseStatus = lens _uarlrsResponseStatus (\ s a -> s{_uarlrsResponseStatus = a})

instance NFData
           UpdateApplicationResourceLifecycleResponse
         where
