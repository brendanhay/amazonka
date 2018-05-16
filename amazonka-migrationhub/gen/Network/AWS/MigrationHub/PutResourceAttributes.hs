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
-- Module      : Network.AWS.MigrationHub.PutResourceAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides identifying details of the resource being migrated so that it can be associated in the Application Discovery Service (ADS)'s repository. This association occurs asynchronously after @PutResourceAttributes@ returns.
--
--
-- /Important:/     * Keep in mind that subsequent calls to PutResourceAttributes will override previously stored attributes. For example, if it is first called with a MAC address, but later, it is desired to /add/ an IP address, it will then be required to call it with /both/ the IP and MAC addresses to prevent overiding the MAC address.
--
--     * Note the instructions regarding the special use case of the @ResourceAttributeList@ parameter when specifying any "VM" related value.
--
--
--
module Network.AWS.MigrationHub.PutResourceAttributes
    (
    -- * Creating a Request
      putResourceAttributes
    , PutResourceAttributes
    -- * Request Lenses
    , praDryRun
    , praProgressUpdateStream
    , praMigrationTaskName
    , praResourceAttributeList

    -- * Destructuring the Response
    , putResourceAttributesResponse
    , PutResourceAttributesResponse
    -- * Response Lenses
    , prarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putResourceAttributes' smart constructor.
data PutResourceAttributes = PutResourceAttributes'
  { _praDryRun                :: !(Maybe Bool)
  , _praProgressUpdateStream  :: !Text
  , _praMigrationTaskName     :: !Text
  , _praResourceAttributeList :: !(List1 ResourceAttribute)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutResourceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'praDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'praProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'praMigrationTaskName' - Unique identifier that references the migration task.
--
-- * 'praResourceAttributeList' - Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service (ADS)'s repository. /Important:/ If any "VM" related value is used for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always used. If it is not used, the server will not be associated in the Application Discovery Service (ADS)'s repository using any of the other "VM" related values, and you will experience data loss. See the Example section below for a use case of specifying "VM" related values.
putResourceAttributes
    :: Text -- ^ 'praProgressUpdateStream'
    -> Text -- ^ 'praMigrationTaskName'
    -> NonEmpty ResourceAttribute -- ^ 'praResourceAttributeList'
    -> PutResourceAttributes
putResourceAttributes pProgressUpdateStream_ pMigrationTaskName_ pResourceAttributeList_ =
  PutResourceAttributes'
    { _praDryRun = Nothing
    , _praProgressUpdateStream = pProgressUpdateStream_
    , _praMigrationTaskName = pMigrationTaskName_
    , _praResourceAttributeList = _List1 # pResourceAttributeList_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
praDryRun :: Lens' PutResourceAttributes (Maybe Bool)
praDryRun = lens _praDryRun (\ s a -> s{_praDryRun = a})

-- | The name of the ProgressUpdateStream.
praProgressUpdateStream :: Lens' PutResourceAttributes Text
praProgressUpdateStream = lens _praProgressUpdateStream (\ s a -> s{_praProgressUpdateStream = a})

-- | Unique identifier that references the migration task.
praMigrationTaskName :: Lens' PutResourceAttributes Text
praMigrationTaskName = lens _praMigrationTaskName (\ s a -> s{_praMigrationTaskName = a})

-- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service (ADS)'s repository. /Important:/ If any "VM" related value is used for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always used. If it is not used, the server will not be associated in the Application Discovery Service (ADS)'s repository using any of the other "VM" related values, and you will experience data loss. See the Example section below for a use case of specifying "VM" related values.
praResourceAttributeList :: Lens' PutResourceAttributes (NonEmpty ResourceAttribute)
praResourceAttributeList = lens _praResourceAttributeList (\ s a -> s{_praResourceAttributeList = a}) . _List1

instance AWSRequest PutResourceAttributes where
        type Rs PutResourceAttributes =
             PutResourceAttributesResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 PutResourceAttributesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable PutResourceAttributes where

instance NFData PutResourceAttributes where

instance ToHeaders PutResourceAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.PutResourceAttributes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutResourceAttributes where
        toJSON PutResourceAttributes'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _praDryRun,
                  Just
                    ("ProgressUpdateStream" .= _praProgressUpdateStream),
                  Just ("MigrationTaskName" .= _praMigrationTaskName),
                  Just
                    ("ResourceAttributeList" .=
                       _praResourceAttributeList)])

instance ToPath PutResourceAttributes where
        toPath = const "/"

instance ToQuery PutResourceAttributes where
        toQuery = const mempty

-- | /See:/ 'putResourceAttributesResponse' smart constructor.
newtype PutResourceAttributesResponse = PutResourceAttributesResponse'
  { _prarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutResourceAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prarsResponseStatus' - -- | The response status code.
putResourceAttributesResponse
    :: Int -- ^ 'prarsResponseStatus'
    -> PutResourceAttributesResponse
putResourceAttributesResponse pResponseStatus_ =
  PutResourceAttributesResponse' {_prarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
prarsResponseStatus :: Lens' PutResourceAttributesResponse Int
prarsResponseStatus = lens _prarsResponseStatus (\ s a -> s{_prarsResponseStatus = a})

instance NFData PutResourceAttributesResponse where
