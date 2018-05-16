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
-- Module      : Network.AWS.Lightsail.CreateInstanceSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a specific virtual private server, or /instance/ . You can use a snapshot to create a new instance that is based on that snapshot.
--
--
module Network.AWS.Lightsail.CreateInstanceSnapshot
    (
    -- * Creating a Request
      createInstanceSnapshot
    , CreateInstanceSnapshot
    -- * Request Lenses
    , cisInstanceSnapshotName
    , cisInstanceName

    -- * Destructuring the Response
    , createInstanceSnapshotResponse
    , CreateInstanceSnapshotResponse
    -- * Response Lenses
    , cisrsOperations
    , cisrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createInstanceSnapshot' smart constructor.
data CreateInstanceSnapshot = CreateInstanceSnapshot'
  { _cisInstanceSnapshotName :: !Text
  , _cisInstanceName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstanceSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisInstanceSnapshotName' - The name for your new snapshot.
--
-- * 'cisInstanceName' - The Lightsail instance on which to base your snapshot.
createInstanceSnapshot
    :: Text -- ^ 'cisInstanceSnapshotName'
    -> Text -- ^ 'cisInstanceName'
    -> CreateInstanceSnapshot
createInstanceSnapshot pInstanceSnapshotName_ pInstanceName_ =
  CreateInstanceSnapshot'
    { _cisInstanceSnapshotName = pInstanceSnapshotName_
    , _cisInstanceName = pInstanceName_
    }


-- | The name for your new snapshot.
cisInstanceSnapshotName :: Lens' CreateInstanceSnapshot Text
cisInstanceSnapshotName = lens _cisInstanceSnapshotName (\ s a -> s{_cisInstanceSnapshotName = a})

-- | The Lightsail instance on which to base your snapshot.
cisInstanceName :: Lens' CreateInstanceSnapshot Text
cisInstanceName = lens _cisInstanceName (\ s a -> s{_cisInstanceName = a})

instance AWSRequest CreateInstanceSnapshot where
        type Rs CreateInstanceSnapshot =
             CreateInstanceSnapshotResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 CreateInstanceSnapshotResponse' <$>
                   (x .?> "operations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable CreateInstanceSnapshot where

instance NFData CreateInstanceSnapshot where

instance ToHeaders CreateInstanceSnapshot where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.CreateInstanceSnapshot" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateInstanceSnapshot where
        toJSON CreateInstanceSnapshot'{..}
          = object
              (catMaybes
                 [Just
                    ("instanceSnapshotName" .= _cisInstanceSnapshotName),
                  Just ("instanceName" .= _cisInstanceName)])

instance ToPath CreateInstanceSnapshot where
        toPath = const "/"

instance ToQuery CreateInstanceSnapshot where
        toQuery = const mempty

-- | /See:/ 'createInstanceSnapshotResponse' smart constructor.
data CreateInstanceSnapshotResponse = CreateInstanceSnapshotResponse'
  { _cisrsOperations     :: !(Maybe [Operation])
  , _cisrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstanceSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisrsOperations' - An array of key-value pairs containing information about the results of your create instances snapshot request.
--
-- * 'cisrsResponseStatus' - -- | The response status code.
createInstanceSnapshotResponse
    :: Int -- ^ 'cisrsResponseStatus'
    -> CreateInstanceSnapshotResponse
createInstanceSnapshotResponse pResponseStatus_ =
  CreateInstanceSnapshotResponse'
    {_cisrsOperations = Nothing, _cisrsResponseStatus = pResponseStatus_}


-- | An array of key-value pairs containing information about the results of your create instances snapshot request.
cisrsOperations :: Lens' CreateInstanceSnapshotResponse [Operation]
cisrsOperations = lens _cisrsOperations (\ s a -> s{_cisrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
cisrsResponseStatus :: Lens' CreateInstanceSnapshotResponse Int
cisrsResponseStatus = lens _cisrsResponseStatus (\ s a -> s{_cisrsResponseStatus = a})

instance NFData CreateInstanceSnapshotResponse where
