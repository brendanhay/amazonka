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
-- Module      : Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels in-progress environment configuration update or application version deployment.
--
--
module Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
    (
    -- * Creating a Request
      abortEnvironmentUpdate
    , AbortEnvironmentUpdate
    -- * Request Lenses
    , aeuEnvironmentName
    , aeuEnvironmentId

    -- * Destructuring the Response
    , abortEnvironmentUpdateResponse
    , AbortEnvironmentUpdateResponse
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'abortEnvironmentUpdate' smart constructor.
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'
  { _aeuEnvironmentName :: !(Maybe Text)
  , _aeuEnvironmentId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AbortEnvironmentUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeuEnvironmentName' - This specifies the name of the environment with the in-progress update that you want to cancel.
--
-- * 'aeuEnvironmentId' - This specifies the ID of the environment with the in-progress update that you want to cancel.
abortEnvironmentUpdate
    :: AbortEnvironmentUpdate
abortEnvironmentUpdate =
  AbortEnvironmentUpdate'
    {_aeuEnvironmentName = Nothing, _aeuEnvironmentId = Nothing}


-- | This specifies the name of the environment with the in-progress update that you want to cancel.
aeuEnvironmentName :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeuEnvironmentName = lens _aeuEnvironmentName (\ s a -> s{_aeuEnvironmentName = a})

-- | This specifies the ID of the environment with the in-progress update that you want to cancel.
aeuEnvironmentId :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeuEnvironmentId = lens _aeuEnvironmentId (\ s a -> s{_aeuEnvironmentId = a})

instance AWSRequest AbortEnvironmentUpdate where
        type Rs AbortEnvironmentUpdate =
             AbortEnvironmentUpdateResponse
        request = postQuery elasticBeanstalk
        response
          = receiveNull AbortEnvironmentUpdateResponse'

instance Hashable AbortEnvironmentUpdate where

instance NFData AbortEnvironmentUpdate where

instance ToHeaders AbortEnvironmentUpdate where
        toHeaders = const mempty

instance ToPath AbortEnvironmentUpdate where
        toPath = const "/"

instance ToQuery AbortEnvironmentUpdate where
        toQuery AbortEnvironmentUpdate'{..}
          = mconcat
              ["Action" =:
                 ("AbortEnvironmentUpdate" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentName" =: _aeuEnvironmentName,
               "EnvironmentId" =: _aeuEnvironmentId]

-- | /See:/ 'abortEnvironmentUpdateResponse' smart constructor.
data AbortEnvironmentUpdateResponse =
  AbortEnvironmentUpdateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AbortEnvironmentUpdateResponse' with the minimum fields required to make a request.
--
abortEnvironmentUpdateResponse
    :: AbortEnvironmentUpdateResponse
abortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'


instance NFData AbortEnvironmentUpdateResponse where
