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
-- Module      : Network.AWS.MigrationHub.DisassociateDiscoveredResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate an Application Discovery Service (ADS) discovered resource from a migration task.
--
--
module Network.AWS.MigrationHub.DisassociateDiscoveredResource
    (
    -- * Creating a Request
      disassociateDiscoveredResource
    , DisassociateDiscoveredResource
    -- * Request Lenses
    , ddrDryRun
    , ddrProgressUpdateStream
    , ddrMigrationTaskName
    , ddrConfigurationId

    -- * Destructuring the Response
    , disassociateDiscoveredResourceResponse
    , DisassociateDiscoveredResourceResponse
    -- * Response Lenses
    , ddrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateDiscoveredResource' smart constructor.
data DisassociateDiscoveredResource = DisassociateDiscoveredResource'
  { _ddrDryRun               :: !(Maybe Bool)
  , _ddrProgressUpdateStream :: !Text
  , _ddrMigrationTaskName    :: !Text
  , _ddrConfigurationId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateDiscoveredResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'ddrProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'ddrMigrationTaskName' - The identifier given to the MigrationTask.
--
-- * 'ddrConfigurationId' - ConfigurationId of the ADS resource to be disassociated.
disassociateDiscoveredResource
    :: Text -- ^ 'ddrProgressUpdateStream'
    -> Text -- ^ 'ddrMigrationTaskName'
    -> Text -- ^ 'ddrConfigurationId'
    -> DisassociateDiscoveredResource
disassociateDiscoveredResource pProgressUpdateStream_ pMigrationTaskName_ pConfigurationId_ =
  DisassociateDiscoveredResource'
    { _ddrDryRun = Nothing
    , _ddrProgressUpdateStream = pProgressUpdateStream_
    , _ddrMigrationTaskName = pMigrationTaskName_
    , _ddrConfigurationId = pConfigurationId_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
ddrDryRun :: Lens' DisassociateDiscoveredResource (Maybe Bool)
ddrDryRun = lens _ddrDryRun (\ s a -> s{_ddrDryRun = a})

-- | The name of the ProgressUpdateStream.
ddrProgressUpdateStream :: Lens' DisassociateDiscoveredResource Text
ddrProgressUpdateStream = lens _ddrProgressUpdateStream (\ s a -> s{_ddrProgressUpdateStream = a})

-- | The identifier given to the MigrationTask.
ddrMigrationTaskName :: Lens' DisassociateDiscoveredResource Text
ddrMigrationTaskName = lens _ddrMigrationTaskName (\ s a -> s{_ddrMigrationTaskName = a})

-- | ConfigurationId of the ADS resource to be disassociated.
ddrConfigurationId :: Lens' DisassociateDiscoveredResource Text
ddrConfigurationId = lens _ddrConfigurationId (\ s a -> s{_ddrConfigurationId = a})

instance AWSRequest DisassociateDiscoveredResource
         where
        type Rs DisassociateDiscoveredResource =
             DisassociateDiscoveredResourceResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateDiscoveredResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateDiscoveredResource
         where

instance NFData DisassociateDiscoveredResource where

instance ToHeaders DisassociateDiscoveredResource
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.DisassociateDiscoveredResource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateDiscoveredResource where
        toJSON DisassociateDiscoveredResource'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _ddrDryRun,
                  Just
                    ("ProgressUpdateStream" .= _ddrProgressUpdateStream),
                  Just ("MigrationTaskName" .= _ddrMigrationTaskName),
                  Just ("ConfigurationId" .= _ddrConfigurationId)])

instance ToPath DisassociateDiscoveredResource where
        toPath = const "/"

instance ToQuery DisassociateDiscoveredResource where
        toQuery = const mempty

-- | /See:/ 'disassociateDiscoveredResourceResponse' smart constructor.
newtype DisassociateDiscoveredResourceResponse = DisassociateDiscoveredResourceResponse'
  { _ddrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateDiscoveredResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddrrsResponseStatus' - -- | The response status code.
disassociateDiscoveredResourceResponse
    :: Int -- ^ 'ddrrsResponseStatus'
    -> DisassociateDiscoveredResourceResponse
disassociateDiscoveredResourceResponse pResponseStatus_ =
  DisassociateDiscoveredResourceResponse'
    {_ddrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ddrrsResponseStatus :: Lens' DisassociateDiscoveredResourceResponse Int
ddrrsResponseStatus = lens _ddrrsResponseStatus (\ s a -> s{_ddrrsResponseStatus = a})

instance NFData
           DisassociateDiscoveredResourceResponse
         where
