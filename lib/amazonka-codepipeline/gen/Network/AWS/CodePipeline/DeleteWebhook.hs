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
-- Module      : Network.AWS.CodePipeline.DeleteWebhook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously created webhook by name. Deleting the webhook stops AWS CodePipeline from starting a pipeline every time an external event occurs. The API will return successfully when trying to delete a webhook that is already deleted. If a deleted webhook is re-created by calling PutWebhook with the same name, it will have a different URL.
--
--
module Network.AWS.CodePipeline.DeleteWebhook
    (
    -- * Creating a Request
      deleteWebhook
    , DeleteWebhook
    -- * Request Lenses
    , dwName

    -- * Destructuring the Response
    , deleteWebhookResponse
    , DeleteWebhookResponse
    -- * Response Lenses
    , dwrsResponseStatus
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteWebhook' smart constructor.
newtype DeleteWebhook = DeleteWebhook'
  { _dwName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWebhook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwName' - The name of the webhook you want to delete.
deleteWebhook
    :: Text -- ^ 'dwName'
    -> DeleteWebhook
deleteWebhook pName_ = DeleteWebhook' {_dwName = pName_}


-- | The name of the webhook you want to delete.
dwName :: Lens' DeleteWebhook Text
dwName = lens _dwName (\ s a -> s{_dwName = a})

instance AWSRequest DeleteWebhook where
        type Rs DeleteWebhook = DeleteWebhookResponse
        request = postJSON codePipeline
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteWebhookResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteWebhook where

instance NFData DeleteWebhook where

instance ToHeaders DeleteWebhook where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.DeleteWebhook" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteWebhook where
        toJSON DeleteWebhook'{..}
          = object (catMaybes [Just ("name" .= _dwName)])

instance ToPath DeleteWebhook where
        toPath = const "/"

instance ToQuery DeleteWebhook where
        toQuery = const mempty

-- | /See:/ 'deleteWebhookResponse' smart constructor.
newtype DeleteWebhookResponse = DeleteWebhookResponse'
  { _dwrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWebhookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwrsResponseStatus' - -- | The response status code.
deleteWebhookResponse
    :: Int -- ^ 'dwrsResponseStatus'
    -> DeleteWebhookResponse
deleteWebhookResponse pResponseStatus_ =
  DeleteWebhookResponse' {_dwrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dwrsResponseStatus :: Lens' DeleteWebhookResponse Int
dwrsResponseStatus = lens _dwrsResponseStatus (\ s a -> s{_dwrsResponseStatus = a})

instance NFData DeleteWebhookResponse where
