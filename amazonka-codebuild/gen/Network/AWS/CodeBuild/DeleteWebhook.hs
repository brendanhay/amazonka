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
-- Module      : Network.AWS.CodeBuild.DeleteWebhook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, stops AWS CodeBuild from automatically rebuilding the source code every time a code change is pushed to the repository.
--
--
module Network.AWS.CodeBuild.DeleteWebhook
    (
    -- * Creating a Request
      deleteWebhook
    , DeleteWebhook
    -- * Request Lenses
    , dwProjectName

    -- * Destructuring the Response
    , deleteWebhookResponse
    , DeleteWebhookResponse
    -- * Response Lenses
    , dwrsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteWebhook' smart constructor.
newtype DeleteWebhook = DeleteWebhook'
  { _dwProjectName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWebhook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwProjectName' - The name of the AWS CodeBuild project.
deleteWebhook
    :: Text -- ^ 'dwProjectName'
    -> DeleteWebhook
deleteWebhook pProjectName_ = DeleteWebhook' {_dwProjectName = pProjectName_}


-- | The name of the AWS CodeBuild project.
dwProjectName :: Lens' DeleteWebhook Text
dwProjectName = lens _dwProjectName (\ s a -> s{_dwProjectName = a})

instance AWSRequest DeleteWebhook where
        type Rs DeleteWebhook = DeleteWebhookResponse
        request = postJSON codeBuild
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
                    ("CodeBuild_20161006.DeleteWebhook" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteWebhook where
        toJSON DeleteWebhook'{..}
          = object
              (catMaybes [Just ("projectName" .= _dwProjectName)])

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
