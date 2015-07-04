{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Cancels in-progress environment configuration update or application
-- version deployment.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_AbortEnvironmentUpdate.html>
module Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
    (
    -- * Request
      AbortEnvironmentUpdate
    -- ** Request constructor
    , abortEnvironmentUpdate
    -- ** Request lenses
    , aeuEnvironmentName
    , aeuEnvironmentId

    -- * Response
    , AbortEnvironmentUpdateResponse
    -- ** Response constructor
    , abortEnvironmentUpdateResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'abortEnvironmentUpdate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aeuEnvironmentName'
--
-- * 'aeuEnvironmentId'
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'
    { _aeuEnvironmentName :: !(Maybe Text)
    , _aeuEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortEnvironmentUpdate' smart constructor.
abortEnvironmentUpdate :: AbortEnvironmentUpdate
abortEnvironmentUpdate =
    AbortEnvironmentUpdate'
    { _aeuEnvironmentName = Nothing
    , _aeuEnvironmentId = Nothing
    }

-- | This specifies the name of the environment with the in-progress update
-- that you want to cancel.
aeuEnvironmentName :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeuEnvironmentName = lens _aeuEnvironmentName (\ s a -> s{_aeuEnvironmentName = a});

-- | This specifies the ID of the environment with the in-progress update
-- that you want to cancel.
aeuEnvironmentId :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeuEnvironmentId = lens _aeuEnvironmentId (\ s a -> s{_aeuEnvironmentId = a});

instance AWSRequest AbortEnvironmentUpdate where
        type Sv AbortEnvironmentUpdate = ElasticBeanstalk
        type Rs AbortEnvironmentUpdate =
             AbortEnvironmentUpdateResponse
        request = post
        response
          = receiveNull AbortEnvironmentUpdateResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortEnvironmentUpdateResponse' smart constructor.
abortEnvironmentUpdateResponse :: AbortEnvironmentUpdateResponse
abortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
