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
-- Module      : Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more on-premises instances.
--
--
module Network.AWS.CodeDeploy.BatchGetOnPremisesInstances
    (
    -- * Creating a Request
      batchGetOnPremisesInstances
    , BatchGetOnPremisesInstances
    -- * Request Lenses
    , bgopiInstanceNames

    -- * Destructuring the Response
    , batchGetOnPremisesInstancesResponse
    , BatchGetOnPremisesInstancesResponse
    -- * Response Lenses
    , bgopirsInstanceInfos
    , bgopirsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a BatchGetOnPremisesInstances operation.
--
--
--
-- /See:/ 'batchGetOnPremisesInstances' smart constructor.
newtype BatchGetOnPremisesInstances = BatchGetOnPremisesInstances'
  { _bgopiInstanceNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetOnPremisesInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgopiInstanceNames' - The names of the on-premises instances about which to get information.
batchGetOnPremisesInstances
    :: BatchGetOnPremisesInstances
batchGetOnPremisesInstances =
  BatchGetOnPremisesInstances' {_bgopiInstanceNames = mempty}


-- | The names of the on-premises instances about which to get information.
bgopiInstanceNames :: Lens' BatchGetOnPremisesInstances [Text]
bgopiInstanceNames = lens _bgopiInstanceNames (\ s a -> s{_bgopiInstanceNames = a}) . _Coerce

instance AWSRequest BatchGetOnPremisesInstances where
        type Rs BatchGetOnPremisesInstances =
             BatchGetOnPremisesInstancesResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetOnPremisesInstancesResponse' <$>
                   (x .?> "instanceInfos" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchGetOnPremisesInstances where

instance NFData BatchGetOnPremisesInstances where

instance ToHeaders BatchGetOnPremisesInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetOnPremisesInstances" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetOnPremisesInstances where
        toJSON BatchGetOnPremisesInstances'{..}
          = object
              (catMaybes
                 [Just ("instanceNames" .= _bgopiInstanceNames)])

instance ToPath BatchGetOnPremisesInstances where
        toPath = const "/"

instance ToQuery BatchGetOnPremisesInstances where
        toQuery = const mempty

-- | Represents the output of a BatchGetOnPremisesInstances operation.
--
--
--
-- /See:/ 'batchGetOnPremisesInstancesResponse' smart constructor.
data BatchGetOnPremisesInstancesResponse = BatchGetOnPremisesInstancesResponse'
  { _bgopirsInstanceInfos  :: !(Maybe [InstanceInfo])
  , _bgopirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetOnPremisesInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgopirsInstanceInfos' - Information about the on-premises instances.
--
-- * 'bgopirsResponseStatus' - -- | The response status code.
batchGetOnPremisesInstancesResponse
    :: Int -- ^ 'bgopirsResponseStatus'
    -> BatchGetOnPremisesInstancesResponse
batchGetOnPremisesInstancesResponse pResponseStatus_ =
  BatchGetOnPremisesInstancesResponse'
    {_bgopirsInstanceInfos = Nothing, _bgopirsResponseStatus = pResponseStatus_}


-- | Information about the on-premises instances.
bgopirsInstanceInfos :: Lens' BatchGetOnPremisesInstancesResponse [InstanceInfo]
bgopirsInstanceInfos = lens _bgopirsInstanceInfos (\ s a -> s{_bgopirsInstanceInfos = a}) . _Default . _Coerce

-- | -- | The response status code.
bgopirsResponseStatus :: Lens' BatchGetOnPremisesInstancesResponse Int
bgopirsResponseStatus = lens _bgopirsResponseStatus (\ s a -> s{_bgopirsResponseStatus = a})

instance NFData BatchGetOnPremisesInstancesResponse
         where
