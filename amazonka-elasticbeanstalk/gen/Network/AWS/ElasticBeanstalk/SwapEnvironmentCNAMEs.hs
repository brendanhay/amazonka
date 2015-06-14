{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
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

-- | Swaps the CNAMEs of two environments.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_SwapEnvironmentCNAMEs.html>
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
    (
    -- * Request
      SwapEnvironmentCNAMEs
    -- ** Request constructor
    , swapEnvironmentCNAMEs
    -- ** Request lenses
    , secnameDestinationEnvironmentId
    , secnameSourceEnvironmentId
    , secnameDestinationEnvironmentName
    , secnameSourceEnvironmentName

    -- * Response
    , SwapEnvironmentCNAMEsResponse
    -- ** Response constructor
    , swapEnvironmentCNAMEsResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticBeanstalk.Types

-- | /See:/ 'swapEnvironmentCNAMEs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'secnameDestinationEnvironmentId'
--
-- * 'secnameSourceEnvironmentId'
--
-- * 'secnameDestinationEnvironmentName'
--
-- * 'secnameSourceEnvironmentName'
data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs'{_secnameDestinationEnvironmentId :: Maybe Text, _secnameSourceEnvironmentId :: Maybe Text, _secnameDestinationEnvironmentName :: Text, _secnameSourceEnvironmentName :: Text} deriving (Eq, Read, Show)

-- | 'SwapEnvironmentCNAMEs' smart constructor.
swapEnvironmentCNAMEs :: Text -> Text -> SwapEnvironmentCNAMEs
swapEnvironmentCNAMEs pDestinationEnvironmentName pSourceEnvironmentName = SwapEnvironmentCNAMEs'{_secnameDestinationEnvironmentId = Nothing, _secnameSourceEnvironmentId = Nothing, _secnameDestinationEnvironmentName = pDestinationEnvironmentName, _secnameSourceEnvironmentName = pSourceEnvironmentName};

-- | The ID of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@.
secnameDestinationEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameDestinationEnvironmentId = lens _secnameDestinationEnvironmentId (\ s a -> s{_secnameDestinationEnvironmentId = a});

-- | The ID of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentId@, you must specify the @DestinationEnvironmentId@.
secnameSourceEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameSourceEnvironmentId = lens _secnameSourceEnvironmentId (\ s a -> s{_secnameSourceEnvironmentId = a});

-- | The name of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentName@ with the
-- @DestinationEnvironmentName@.
secnameDestinationEnvironmentName :: Lens' SwapEnvironmentCNAMEs Text
secnameDestinationEnvironmentName = lens _secnameDestinationEnvironmentName (\ s a -> s{_secnameDestinationEnvironmentName = a});

-- | The name of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentName@, you must specify the
-- @DestinationEnvironmentName@.
secnameSourceEnvironmentName :: Lens' SwapEnvironmentCNAMEs Text
secnameSourceEnvironmentName = lens _secnameSourceEnvironmentName (\ s a -> s{_secnameSourceEnvironmentName = a});

instance AWSRequest SwapEnvironmentCNAMEs where
        type Sv SwapEnvironmentCNAMEs = ElasticBeanstalk
        type Rs SwapEnvironmentCNAMEs =
             SwapEnvironmentCNAMEsResponse
        request = post
        response = receiveNull SwapEnvironmentCNAMEsResponse'

instance ToHeaders SwapEnvironmentCNAMEs where
        toHeaders = const mempty

instance ToPath SwapEnvironmentCNAMEs where
        toPath = const "/"

instance ToQuery SwapEnvironmentCNAMEs where
        toQuery SwapEnvironmentCNAMEs'{..}
          = mconcat
              ["Action" =: ("SwapEnvironmentCNAMEs" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "DestinationEnvironmentId" =:
                 _secnameDestinationEnvironmentId,
               "SourceEnvironmentId" =: _secnameSourceEnvironmentId,
               "DestinationEnvironmentName" =:
                 _secnameDestinationEnvironmentName,
               "SourceEnvironmentName" =:
                 _secnameSourceEnvironmentName]

-- | /See:/ 'swapEnvironmentCNAMEsResponse' smart constructor.
data SwapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse' deriving (Eq, Read, Show)

-- | 'SwapEnvironmentCNAMEsResponse' smart constructor.
swapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse
swapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse';
