{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Swaps the CNAMEs of two environments.
--
-- <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_SwapEnvironmentCNAMEs.html>
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
    (
    -- * Request
      SwapEnvironmentCNAMEs
    -- ** Request constructor
    , swapEnvironmentCNAMEs
    -- ** Request lenses
    , secnamerqDestinationEnvironmentName
    , secnamerqDestinationEnvironmentId
    , secnamerqSourceEnvironmentName
    , secnamerqSourceEnvironmentId

    -- * Response
    , SwapEnvironmentCNAMEsResponse
    -- ** Response constructor
    , swapEnvironmentCNAMEsResponse
    ) where

import           Network.AWS.ElasticBeanstalk.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Swaps the CNAMEs of two environments.
--
-- /See:/ 'swapEnvironmentCNAMEs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'secnamerqDestinationEnvironmentName'
--
-- * 'secnamerqDestinationEnvironmentId'
--
-- * 'secnamerqSourceEnvironmentName'
--
-- * 'secnamerqSourceEnvironmentId'
data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs'
    { _secnamerqDestinationEnvironmentName :: !(Maybe Text)
    , _secnamerqDestinationEnvironmentId   :: !(Maybe Text)
    , _secnamerqSourceEnvironmentName      :: !(Maybe Text)
    , _secnamerqSourceEnvironmentId        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SwapEnvironmentCNAMEs' smart constructor.
swapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs
swapEnvironmentCNAMEs =
    SwapEnvironmentCNAMEs'
    { _secnamerqDestinationEnvironmentName = Nothing
    , _secnamerqDestinationEnvironmentId = Nothing
    , _secnamerqSourceEnvironmentName = Nothing
    , _secnamerqSourceEnvironmentId = Nothing
    }

-- | The name of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentName@ with the
-- @DestinationEnvironmentName@.
secnamerqDestinationEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnamerqDestinationEnvironmentName = lens _secnamerqDestinationEnvironmentName (\ s a -> s{_secnamerqDestinationEnvironmentName = a});

-- | The ID of the destination environment.
--
-- Condition: You must specify at least the @DestinationEnvironmentID@ or
-- the @DestinationEnvironmentName@. You may also specify both. You must
-- specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@.
secnamerqDestinationEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnamerqDestinationEnvironmentId = lens _secnamerqDestinationEnvironmentId (\ s a -> s{_secnamerqDestinationEnvironmentId = a});

-- | The name of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentName@, you must specify the
-- @DestinationEnvironmentName@.
secnamerqSourceEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnamerqSourceEnvironmentName = lens _secnamerqSourceEnvironmentName (\ s a -> s{_secnamerqSourceEnvironmentName = a});

-- | The ID of the source environment.
--
-- Condition: You must specify at least the @SourceEnvironmentID@ or the
-- @SourceEnvironmentName@. You may also specify both. If you specify the
-- @SourceEnvironmentId@, you must specify the @DestinationEnvironmentId@.
secnamerqSourceEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnamerqSourceEnvironmentId = lens _secnamerqSourceEnvironmentId (\ s a -> s{_secnamerqSourceEnvironmentId = a});

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
               "DestinationEnvironmentName" =:
                 _secnamerqDestinationEnvironmentName,
               "DestinationEnvironmentId" =:
                 _secnamerqDestinationEnvironmentId,
               "SourceEnvironmentName" =:
                 _secnamerqSourceEnvironmentName,
               "SourceEnvironmentId" =:
                 _secnamerqSourceEnvironmentId]

-- | /See:/ 'swapEnvironmentCNAMEsResponse' smart constructor.
data SwapEnvironmentCNAMEsResponse =
    SwapEnvironmentCNAMEsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SwapEnvironmentCNAMEsResponse' smart constructor.
swapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse
swapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse'
