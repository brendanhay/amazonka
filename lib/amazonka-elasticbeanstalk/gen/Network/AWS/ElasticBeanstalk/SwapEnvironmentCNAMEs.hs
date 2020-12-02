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
-- Module      : Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Swaps the CNAMEs of two environments.
--
--
module Network.AWS.ElasticBeanstalk.SwapEnvironmentCNAMEs
    (
    -- * Creating a Request
      swapEnvironmentCNAMEs
    , SwapEnvironmentCNAMEs
    -- * Request Lenses
    , secnameDestinationEnvironmentName
    , secnameDestinationEnvironmentId
    , secnameSourceEnvironmentName
    , secnameSourceEnvironmentId

    -- * Destructuring the Response
    , swapEnvironmentCNAMEsResponse
    , SwapEnvironmentCNAMEsResponse
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Swaps the CNAMEs of two environments.
--
--
--
-- /See:/ 'swapEnvironmentCNAMEs' smart constructor.
data SwapEnvironmentCNAMEs = SwapEnvironmentCNAMEs'
  { _secnameDestinationEnvironmentName :: !(Maybe Text)
  , _secnameDestinationEnvironmentId   :: !(Maybe Text)
  , _secnameSourceEnvironmentName      :: !(Maybe Text)
  , _secnameSourceEnvironmentId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SwapEnvironmentCNAMEs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'secnameDestinationEnvironmentName' - The name of the destination environment. Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentName@ with the @DestinationEnvironmentName@ .
--
-- * 'secnameDestinationEnvironmentId' - The ID of the destination environment. Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@ .
--
-- * 'secnameSourceEnvironmentName' - The name of the source environment. Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentName@ , you must specify the @DestinationEnvironmentName@ .
--
-- * 'secnameSourceEnvironmentId' - The ID of the source environment. Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentId@ , you must specify the @DestinationEnvironmentId@ .
swapEnvironmentCNAMEs
    :: SwapEnvironmentCNAMEs
swapEnvironmentCNAMEs =
  SwapEnvironmentCNAMEs'
    { _secnameDestinationEnvironmentName = Nothing
    , _secnameDestinationEnvironmentId = Nothing
    , _secnameSourceEnvironmentName = Nothing
    , _secnameSourceEnvironmentId = Nothing
    }


-- | The name of the destination environment. Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentName@ with the @DestinationEnvironmentName@ .
secnameDestinationEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameDestinationEnvironmentName = lens _secnameDestinationEnvironmentName (\ s a -> s{_secnameDestinationEnvironmentName = a})

-- | The ID of the destination environment. Condition: You must specify at least the @DestinationEnvironmentID@ or the @DestinationEnvironmentName@ . You may also specify both. You must specify the @SourceEnvironmentId@ with the @DestinationEnvironmentId@ .
secnameDestinationEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameDestinationEnvironmentId = lens _secnameDestinationEnvironmentId (\ s a -> s{_secnameDestinationEnvironmentId = a})

-- | The name of the source environment. Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentName@ , you must specify the @DestinationEnvironmentName@ .
secnameSourceEnvironmentName :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameSourceEnvironmentName = lens _secnameSourceEnvironmentName (\ s a -> s{_secnameSourceEnvironmentName = a})

-- | The ID of the source environment. Condition: You must specify at least the @SourceEnvironmentID@ or the @SourceEnvironmentName@ . You may also specify both. If you specify the @SourceEnvironmentId@ , you must specify the @DestinationEnvironmentId@ .
secnameSourceEnvironmentId :: Lens' SwapEnvironmentCNAMEs (Maybe Text)
secnameSourceEnvironmentId = lens _secnameSourceEnvironmentId (\ s a -> s{_secnameSourceEnvironmentId = a})

instance AWSRequest SwapEnvironmentCNAMEs where
        type Rs SwapEnvironmentCNAMEs =
             SwapEnvironmentCNAMEsResponse
        request = postQuery elasticBeanstalk
        response = receiveNull SwapEnvironmentCNAMEsResponse'

instance Hashable SwapEnvironmentCNAMEs where

instance NFData SwapEnvironmentCNAMEs where

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
                 _secnameDestinationEnvironmentName,
               "DestinationEnvironmentId" =:
                 _secnameDestinationEnvironmentId,
               "SourceEnvironmentName" =:
                 _secnameSourceEnvironmentName,
               "SourceEnvironmentId" =: _secnameSourceEnvironmentId]

-- | /See:/ 'swapEnvironmentCNAMEsResponse' smart constructor.
data SwapEnvironmentCNAMEsResponse =
  SwapEnvironmentCNAMEsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SwapEnvironmentCNAMEsResponse' with the minimum fields required to make a request.
--
swapEnvironmentCNAMEsResponse
    :: SwapEnvironmentCNAMEsResponse
swapEnvironmentCNAMEsResponse = SwapEnvironmentCNAMEsResponse'


instance NFData SwapEnvironmentCNAMEsResponse where
