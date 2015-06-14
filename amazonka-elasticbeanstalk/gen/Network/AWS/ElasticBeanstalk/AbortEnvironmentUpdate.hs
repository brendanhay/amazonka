{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
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
    , aeuEnvironmentId
    , aeuEnvironmentName

    -- * Response
    , AbortEnvironmentUpdateResponse
    -- ** Response constructor
    , abortEnvironmentUpdateResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'abortEnvironmentUpdate' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aeuEnvironmentId'
--
-- * 'aeuEnvironmentName'
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'{_aeuEnvironmentId :: Maybe Text, _aeuEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'AbortEnvironmentUpdate' smart constructor.
abortEnvironmentUpdate :: Text -> AbortEnvironmentUpdate
abortEnvironmentUpdate pEnvironmentName = AbortEnvironmentUpdate'{_aeuEnvironmentId = Nothing, _aeuEnvironmentName = pEnvironmentName};

-- | This specifies the ID of the environment with the in-progress update
-- that you want to cancel.
aeuEnvironmentId :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeuEnvironmentId = lens _aeuEnvironmentId (\ s a -> s{_aeuEnvironmentId = a});

-- | This specifies the name of the environment with the in-progress update
-- that you want to cancel.
aeuEnvironmentName :: Lens' AbortEnvironmentUpdate Text
aeuEnvironmentName = lens _aeuEnvironmentName (\ s a -> s{_aeuEnvironmentName = a});

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
               "EnvironmentId" =: _aeuEnvironmentId,
               "EnvironmentName" =: _aeuEnvironmentName]

-- | /See:/ 'abortEnvironmentUpdateResponse' smart constructor.
data AbortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse' deriving (Eq, Read, Show)

-- | 'AbortEnvironmentUpdateResponse' smart constructor.
abortEnvironmentUpdateResponse :: AbortEnvironmentUpdateResponse
abortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse';
