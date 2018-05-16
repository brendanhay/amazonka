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
-- Module      : Network.AWS.CodeBuild.UpdateWebhook
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the webhook associated with an AWS CodeBuild build project.
--
--
module Network.AWS.CodeBuild.UpdateWebhook
    (
    -- * Creating a Request
      updateWebhook
    , UpdateWebhook
    -- * Request Lenses
    , uwBranchFilter
    , uwRotateSecret
    , uwProjectName

    -- * Destructuring the Response
    , updateWebhookResponse
    , UpdateWebhookResponse
    -- * Response Lenses
    , uwrsWebhook
    , uwrsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateWebhook' smart constructor.
data UpdateWebhook = UpdateWebhook'
  { _uwBranchFilter :: !(Maybe Text)
  , _uwRotateSecret :: !(Maybe Bool)
  , _uwProjectName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateWebhook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwBranchFilter' - A regular expression used to determine which branches in a repository are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If it doesn't match, then it is not. If branchFilter is empty, then all branches are built.
--
-- * 'uwRotateSecret' - A boolean value that specifies whether the associated repository's secret token should be updated.
--
-- * 'uwProjectName' - The name of the AWS CodeBuild project.
updateWebhook
    :: Text -- ^ 'uwProjectName'
    -> UpdateWebhook
updateWebhook pProjectName_ =
  UpdateWebhook'
    { _uwBranchFilter = Nothing
    , _uwRotateSecret = Nothing
    , _uwProjectName = pProjectName_
    }


-- | A regular expression used to determine which branches in a repository are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If it doesn't match, then it is not. If branchFilter is empty, then all branches are built.
uwBranchFilter :: Lens' UpdateWebhook (Maybe Text)
uwBranchFilter = lens _uwBranchFilter (\ s a -> s{_uwBranchFilter = a})

-- | A boolean value that specifies whether the associated repository's secret token should be updated.
uwRotateSecret :: Lens' UpdateWebhook (Maybe Bool)
uwRotateSecret = lens _uwRotateSecret (\ s a -> s{_uwRotateSecret = a})

-- | The name of the AWS CodeBuild project.
uwProjectName :: Lens' UpdateWebhook Text
uwProjectName = lens _uwProjectName (\ s a -> s{_uwProjectName = a})

instance AWSRequest UpdateWebhook where
        type Rs UpdateWebhook = UpdateWebhookResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 UpdateWebhookResponse' <$>
                   (x .?> "webhook") <*> (pure (fromEnum s)))

instance Hashable UpdateWebhook where

instance NFData UpdateWebhook where

instance ToHeaders UpdateWebhook where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.UpdateWebhook" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateWebhook where
        toJSON UpdateWebhook'{..}
          = object
              (catMaybes
                 [("branchFilter" .=) <$> _uwBranchFilter,
                  ("rotateSecret" .=) <$> _uwRotateSecret,
                  Just ("projectName" .= _uwProjectName)])

instance ToPath UpdateWebhook where
        toPath = const "/"

instance ToQuery UpdateWebhook where
        toQuery = const mempty

-- | /See:/ 'updateWebhookResponse' smart constructor.
data UpdateWebhookResponse = UpdateWebhookResponse'
  { _uwrsWebhook        :: !(Maybe Webhook)
  , _uwrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateWebhookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwrsWebhook' - Information about a repository's webhook that is associated with a project in AWS CodeBuild.
--
-- * 'uwrsResponseStatus' - -- | The response status code.
updateWebhookResponse
    :: Int -- ^ 'uwrsResponseStatus'
    -> UpdateWebhookResponse
updateWebhookResponse pResponseStatus_ =
  UpdateWebhookResponse'
    {_uwrsWebhook = Nothing, _uwrsResponseStatus = pResponseStatus_}


-- | Information about a repository's webhook that is associated with a project in AWS CodeBuild.
uwrsWebhook :: Lens' UpdateWebhookResponse (Maybe Webhook)
uwrsWebhook = lens _uwrsWebhook (\ s a -> s{_uwrsWebhook = a})

-- | -- | The response status code.
uwrsResponseStatus :: Lens' UpdateWebhookResponse Int
uwrsResponseStatus = lens _uwrsResponseStatus (\ s a -> s{_uwrsResponseStatus = a})

instance NFData UpdateWebhookResponse where
