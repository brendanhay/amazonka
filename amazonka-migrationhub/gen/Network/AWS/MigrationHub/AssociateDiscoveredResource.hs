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
-- Module      : Network.AWS.MigrationHub.AssociateDiscoveredResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a discovered resource ID from Application Discovery Service (ADS) with a migration task.
--
--
module Network.AWS.MigrationHub.AssociateDiscoveredResource
    (
    -- * Creating a Request
      associateDiscoveredResource
    , AssociateDiscoveredResource
    -- * Request Lenses
    , adrDryRun
    , adrProgressUpdateStream
    , adrMigrationTaskName
    , adrDiscoveredResource

    -- * Destructuring the Response
    , associateDiscoveredResourceResponse
    , AssociateDiscoveredResourceResponse
    -- * Response Lenses
    , adrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateDiscoveredResource' smart constructor.
data AssociateDiscoveredResource = AssociateDiscoveredResource'
  { _adrDryRun               :: !(Maybe Bool)
  , _adrProgressUpdateStream :: !Text
  , _adrMigrationTaskName    :: !Text
  , _adrDiscoveredResource   :: !DiscoveredResource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDiscoveredResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'adrProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'adrMigrationTaskName' - The identifier given to the MigrationTask.
--
-- * 'adrDiscoveredResource' - Object representing a Resource.
associateDiscoveredResource
    :: Text -- ^ 'adrProgressUpdateStream'
    -> Text -- ^ 'adrMigrationTaskName'
    -> DiscoveredResource -- ^ 'adrDiscoveredResource'
    -> AssociateDiscoveredResource
associateDiscoveredResource pProgressUpdateStream_ pMigrationTaskName_ pDiscoveredResource_ =
  AssociateDiscoveredResource'
    { _adrDryRun = Nothing
    , _adrProgressUpdateStream = pProgressUpdateStream_
    , _adrMigrationTaskName = pMigrationTaskName_
    , _adrDiscoveredResource = pDiscoveredResource_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
adrDryRun :: Lens' AssociateDiscoveredResource (Maybe Bool)
adrDryRun = lens _adrDryRun (\ s a -> s{_adrDryRun = a})

-- | The name of the ProgressUpdateStream.
adrProgressUpdateStream :: Lens' AssociateDiscoveredResource Text
adrProgressUpdateStream = lens _adrProgressUpdateStream (\ s a -> s{_adrProgressUpdateStream = a})

-- | The identifier given to the MigrationTask.
adrMigrationTaskName :: Lens' AssociateDiscoveredResource Text
adrMigrationTaskName = lens _adrMigrationTaskName (\ s a -> s{_adrMigrationTaskName = a})

-- | Object representing a Resource.
adrDiscoveredResource :: Lens' AssociateDiscoveredResource DiscoveredResource
adrDiscoveredResource = lens _adrDiscoveredResource (\ s a -> s{_adrDiscoveredResource = a})

instance AWSRequest AssociateDiscoveredResource where
        type Rs AssociateDiscoveredResource =
             AssociateDiscoveredResourceResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateDiscoveredResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateDiscoveredResource where

instance NFData AssociateDiscoveredResource where

instance ToHeaders AssociateDiscoveredResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.AssociateDiscoveredResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateDiscoveredResource where
        toJSON AssociateDiscoveredResource'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _adrDryRun,
                  Just
                    ("ProgressUpdateStream" .= _adrProgressUpdateStream),
                  Just ("MigrationTaskName" .= _adrMigrationTaskName),
                  Just
                    ("DiscoveredResource" .= _adrDiscoveredResource)])

instance ToPath AssociateDiscoveredResource where
        toPath = const "/"

instance ToQuery AssociateDiscoveredResource where
        toQuery = const mempty

-- | /See:/ 'associateDiscoveredResourceResponse' smart constructor.
newtype AssociateDiscoveredResourceResponse = AssociateDiscoveredResourceResponse'
  { _adrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateDiscoveredResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adrrsResponseStatus' - -- | The response status code.
associateDiscoveredResourceResponse
    :: Int -- ^ 'adrrsResponseStatus'
    -> AssociateDiscoveredResourceResponse
associateDiscoveredResourceResponse pResponseStatus_ =
  AssociateDiscoveredResourceResponse' {_adrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
adrrsResponseStatus :: Lens' AssociateDiscoveredResourceResponse Int
adrrsResponseStatus = lens _adrrsResponseStatus (\ s a -> s{_adrrsResponseStatus = a})

instance NFData AssociateDiscoveredResourceResponse
         where
