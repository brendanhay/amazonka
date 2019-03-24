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
-- Module      : Network.AWS.Connect.UpdateContactAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateContactAttributes@ operation lets you programmatically create new, or update existing, contact attributes associated with a contact. You can use the operation to add or update attributes for both ongoing and completed contacts. For example, you can update the customer's name or the reason the customer called while the call is active, or add notes about steps that the agent took during the call that are displayed to the next agent that takes the call. You can also use the @UpdateContactAttributes@ operation to update attributes for a contact using data from your CRM application and save the data with the contact in Amazon Connect. You could also flag calls for additional analysis, such as legal review or identifying abusive callers.
--
--
-- Contact attributes are available in Amazon Connect for 24 months, and are then deleted.
--
-- /Important:/
--
-- You cannot use the operation to update attributes for contacts that occurred prior to the release of the API, September 12, 2018. You can update attributes only for contacts that started after the release of the API. If you attempt to update attributes for a contact that occurred prior to the release of the API, a 400 error is returned. This applies also to queued callbacks that were initiated prior to the release of the API but are still active in your instance.
--
module Network.AWS.Connect.UpdateContactAttributes
    (
    -- * Creating a Request
      updateContactAttributes
    , UpdateContactAttributes
    -- * Request Lenses
    , ucaInitialContactId
    , ucaInstanceId
    , ucaAttributes

    -- * Destructuring the Response
    , updateContactAttributesResponse
    , UpdateContactAttributesResponse
    -- * Response Lenses
    , ucarsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateContactAttributes' smart constructor.
data UpdateContactAttributes = UpdateContactAttributes'
  { _ucaInitialContactId :: !Text
  , _ucaInstanceId       :: !Text
  , _ucaAttributes       :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContactAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucaInitialContactId' - The unique identifier of the contact for which to update attributes. This is the identifier for the contact associated with the first interaction with the contact center.
--
-- * 'ucaInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
--
-- * 'ucaAttributes' - Specify a custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes. There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
updateContactAttributes
    :: Text -- ^ 'ucaInitialContactId'
    -> Text -- ^ 'ucaInstanceId'
    -> UpdateContactAttributes
updateContactAttributes pInitialContactId_ pInstanceId_ =
  UpdateContactAttributes'
    { _ucaInitialContactId = pInitialContactId_
    , _ucaInstanceId = pInstanceId_
    , _ucaAttributes = mempty
    }


-- | The unique identifier of the contact for which to update attributes. This is the identifier for the contact associated with the first interaction with the contact center.
ucaInitialContactId :: Lens' UpdateContactAttributes Text
ucaInitialContactId = lens _ucaInitialContactId (\ s a -> s{_ucaInitialContactId = a})

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
ucaInstanceId :: Lens' UpdateContactAttributes Text
ucaInstanceId = lens _ucaInstanceId (\ s a -> s{_ucaInstanceId = a})

-- | Specify a custom key-value pair using an attribute map. The attributes are standard Amazon Connect attributes, and can be accessed in contact flows just like any other contact attributes. There can be up to 32,768 UTF-8 bytes across all key-value pairs per contact. Attribute keys can include only alphanumeric, dash, and underscore characters.
ucaAttributes :: Lens' UpdateContactAttributes (HashMap Text Text)
ucaAttributes = lens _ucaAttributes (\ s a -> s{_ucaAttributes = a}) . _Map

instance AWSRequest UpdateContactAttributes where
        type Rs UpdateContactAttributes =
             UpdateContactAttributesResponse
        request = postJSON connect
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateContactAttributesResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateContactAttributes where

instance NFData UpdateContactAttributes where

instance ToHeaders UpdateContactAttributes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateContactAttributes where
        toJSON UpdateContactAttributes'{..}
          = object
              (catMaybes
                 [Just ("InitialContactId" .= _ucaInitialContactId),
                  Just ("InstanceId" .= _ucaInstanceId),
                  Just ("Attributes" .= _ucaAttributes)])

instance ToPath UpdateContactAttributes where
        toPath = const "/contact/attributes"

instance ToQuery UpdateContactAttributes where
        toQuery = const mempty

-- | /See:/ 'updateContactAttributesResponse' smart constructor.
newtype UpdateContactAttributesResponse = UpdateContactAttributesResponse'
  { _ucarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContactAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucarsResponseStatus' - -- | The response status code.
updateContactAttributesResponse
    :: Int -- ^ 'ucarsResponseStatus'
    -> UpdateContactAttributesResponse
updateContactAttributesResponse pResponseStatus_ =
  UpdateContactAttributesResponse' {_ucarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucarsResponseStatus :: Lens' UpdateContactAttributesResponse Int
ucarsResponseStatus = lens _ucarsResponseStatus (\ s a -> s{_ucarsResponseStatus = a})

instance NFData UpdateContactAttributesResponse where
