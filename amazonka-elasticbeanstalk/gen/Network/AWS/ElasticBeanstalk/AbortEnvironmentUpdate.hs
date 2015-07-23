{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.AbortEnvironmentUpdate
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels in-progress environment configuration update or application
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
    , aeurqEnvironmentName
    , aeurqEnvironmentId

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
-- * 'aeurqEnvironmentName'
--
-- * 'aeurqEnvironmentId'
data AbortEnvironmentUpdate = AbortEnvironmentUpdate'
    { _aeurqEnvironmentName :: !(Maybe Text)
    , _aeurqEnvironmentId   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortEnvironmentUpdate' smart constructor.
abortEnvironmentUpdate :: AbortEnvironmentUpdate
abortEnvironmentUpdate =
    AbortEnvironmentUpdate'
    { _aeurqEnvironmentName = Nothing
    , _aeurqEnvironmentId = Nothing
    }

-- | This specifies the name of the environment with the in-progress update
-- that you want to cancel.
aeurqEnvironmentName :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeurqEnvironmentName = lens _aeurqEnvironmentName (\ s a -> s{_aeurqEnvironmentName = a});

-- | This specifies the ID of the environment with the in-progress update
-- that you want to cancel.
aeurqEnvironmentId :: Lens' AbortEnvironmentUpdate (Maybe Text)
aeurqEnvironmentId = lens _aeurqEnvironmentId (\ s a -> s{_aeurqEnvironmentId = a});

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
               "EnvironmentName" =: _aeurqEnvironmentName,
               "EnvironmentId" =: _aeurqEnvironmentId]

-- | /See:/ 'abortEnvironmentUpdateResponse' smart constructor.
data AbortEnvironmentUpdateResponse =
    AbortEnvironmentUpdateResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AbortEnvironmentUpdateResponse' smart constructor.
abortEnvironmentUpdateResponse :: AbortEnvironmentUpdateResponse
abortEnvironmentUpdateResponse = AbortEnvironmentUpdateResponse'
