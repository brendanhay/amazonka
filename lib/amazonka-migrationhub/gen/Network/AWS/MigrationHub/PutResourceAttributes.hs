{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.PutResourceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides identifying details of the resource being migrated so that it can be associated in the Application Discovery Service repository. This association occurs asynchronously after @PutResourceAttributes@ returns.
--
--
-- /Important:/     * Keep in mind that subsequent calls to PutResourceAttributes will override previously stored attributes. For example, if it is first called with a MAC address, but later, it is desired to /add/ an IP address, it will then be required to call it with /both/ the IP and MAC addresses to prevent overriding the MAC address.
--
--     * Note the instructions regarding the special use case of the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#migrationhub-PutResourceAttributes-request-ResourceAttributeList @ResourceAttributeList@ > parameter when specifying any "VM" related value.
module Network.AWS.MigrationHub.PutResourceAttributes
  ( -- * Creating a Request
    putResourceAttributes,
    PutResourceAttributes,

    -- * Request Lenses
    praDryRun,
    praProgressUpdateStream,
    praMigrationTaskName,
    praResourceAttributeList,

    -- * Destructuring the Response
    putResourceAttributesResponse,
    PutResourceAttributesResponse,

    -- * Response Lenses
    prarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putResourceAttributes' smart constructor.
data PutResourceAttributes = PutResourceAttributes'
  { _praDryRun ::
      !(Maybe Bool),
    _praProgressUpdateStream :: !Text,
    _praMigrationTaskName :: !Text,
    _praResourceAttributeList ::
      !(List1 ResourceAttribute)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutResourceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'praDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'praProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'praMigrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- * 'praResourceAttributeList' - Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository. /Important:/     * If any "VM" related value is set for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always set. If @VM_MANAGER_ID@ is not set, then all "VM" fields will be discarded and "VM" fields will not be used for matching the migration task to a server in Application Discovery Service repository. See the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example> section below for a use case of specifying "VM" related values.     * If a server you are trying to match has multiple IP or MAC addresses, you should provide as many as you know in separate type/value pairs passed to the @ResourceAttributeList@ parameter to maximize the chances of matching.
putResourceAttributes ::
  -- | 'praProgressUpdateStream'
  Text ->
  -- | 'praMigrationTaskName'
  Text ->
  -- | 'praResourceAttributeList'
  NonEmpty ResourceAttribute ->
  PutResourceAttributes
putResourceAttributes
  pProgressUpdateStream_
  pMigrationTaskName_
  pResourceAttributeList_ =
    PutResourceAttributes'
      { _praDryRun = Nothing,
        _praProgressUpdateStream = pProgressUpdateStream_,
        _praMigrationTaskName = pMigrationTaskName_,
        _praResourceAttributeList = _List1 # pResourceAttributeList_
      }

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
praDryRun :: Lens' PutResourceAttributes (Maybe Bool)
praDryRun = lens _praDryRun (\s a -> s {_praDryRun = a})

-- | The name of the ProgressUpdateStream.
praProgressUpdateStream :: Lens' PutResourceAttributes Text
praProgressUpdateStream = lens _praProgressUpdateStream (\s a -> s {_praProgressUpdateStream = a})

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
praMigrationTaskName :: Lens' PutResourceAttributes Text
praMigrationTaskName = lens _praMigrationTaskName (\s a -> s {_praMigrationTaskName = a})

-- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository. /Important:/     * If any "VM" related value is set for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always set. If @VM_MANAGER_ID@ is not set, then all "VM" fields will be discarded and "VM" fields will not be used for matching the migration task to a server in Application Discovery Service repository. See the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example> section below for a use case of specifying "VM" related values.     * If a server you are trying to match has multiple IP or MAC addresses, you should provide as many as you know in separate type/value pairs passed to the @ResourceAttributeList@ parameter to maximize the chances of matching.
praResourceAttributeList :: Lens' PutResourceAttributes (NonEmpty ResourceAttribute)
praResourceAttributeList = lens _praResourceAttributeList (\s a -> s {_praResourceAttributeList = a}) . _List1

instance AWSRequest PutResourceAttributes where
  type Rs PutResourceAttributes = PutResourceAttributesResponse
  request = postJSON migrationHub
  response =
    receiveEmpty
      (\s h x -> PutResourceAttributesResponse' <$> (pure (fromEnum s)))

instance Hashable PutResourceAttributes

instance NFData PutResourceAttributes

instance ToHeaders PutResourceAttributes where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSMigrationHub.PutResourceAttributes" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutResourceAttributes where
  toJSON PutResourceAttributes' {..} =
    object
      ( catMaybes
          [ ("DryRun" .=) <$> _praDryRun,
            Just ("ProgressUpdateStream" .= _praProgressUpdateStream),
            Just ("MigrationTaskName" .= _praMigrationTaskName),
            Just ("ResourceAttributeList" .= _praResourceAttributeList)
          ]
      )

instance ToPath PutResourceAttributes where
  toPath = const "/"

instance ToQuery PutResourceAttributes where
  toQuery = const mempty

-- | /See:/ 'putResourceAttributesResponse' smart constructor.
newtype PutResourceAttributesResponse = PutResourceAttributesResponse'
  { _prarsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutResourceAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prarsResponseStatus' - -- | The response status code.
putResourceAttributesResponse ::
  -- | 'prarsResponseStatus'
  Int ->
  PutResourceAttributesResponse
putResourceAttributesResponse pResponseStatus_ =
  PutResourceAttributesResponse'
    { _prarsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
prarsResponseStatus :: Lens' PutResourceAttributesResponse Int
prarsResponseStatus = lens _prarsResponseStatus (\s a -> s {_prarsResponseStatus = a})

instance NFData PutResourceAttributesResponse
