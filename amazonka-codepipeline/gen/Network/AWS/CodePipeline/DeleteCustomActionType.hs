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
-- Module      : Network.AWS.CodePipeline.DeleteCustomActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks a custom action as deleted. PollForJobs for the custom action will fail after the action is marked for deletion. Only used for custom actions.
--
--
-- /Important:/ To re-create a custom action after it has been deleted you must use a string in the version field that has never been used before. This string can be an incremented version number, for example. To restore a deleted custom action, use a JSON file that is identical to the deleted action, including the original string in the version field.
--
module Network.AWS.CodePipeline.DeleteCustomActionType
    (
    -- * Creating a Request
      deleteCustomActionType
    , DeleteCustomActionType
    -- * Request Lenses
    , dcatCategory
    , dcatProvider
    , dcatVersion

    -- * Destructuring the Response
    , deleteCustomActionTypeResponse
    , DeleteCustomActionTypeResponse
    ) where

import Network.AWS.CodePipeline.Types
import Network.AWS.CodePipeline.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a DeleteCustomActionType operation. The custom action will be marked as deleted.
--
--
--
-- /See:/ 'deleteCustomActionType' smart constructor.
data DeleteCustomActionType = DeleteCustomActionType'
  { _dcatCategory :: !ActionCategory
  , _dcatProvider :: !Text
  , _dcatVersion  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCustomActionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcatCategory' - The category of the custom action that you want to delete, such as source or deploy.
--
-- * 'dcatProvider' - The provider of the service used in the custom action, such as AWS CodeDeploy.
--
-- * 'dcatVersion' - The version of the custom action to delete.
deleteCustomActionType
    :: ActionCategory -- ^ 'dcatCategory'
    -> Text -- ^ 'dcatProvider'
    -> Text -- ^ 'dcatVersion'
    -> DeleteCustomActionType
deleteCustomActionType pCategory_ pProvider_ pVersion_ =
  DeleteCustomActionType'
    { _dcatCategory = pCategory_
    , _dcatProvider = pProvider_
    , _dcatVersion = pVersion_
    }


-- | The category of the custom action that you want to delete, such as source or deploy.
dcatCategory :: Lens' DeleteCustomActionType ActionCategory
dcatCategory = lens _dcatCategory (\ s a -> s{_dcatCategory = a})

-- | The provider of the service used in the custom action, such as AWS CodeDeploy.
dcatProvider :: Lens' DeleteCustomActionType Text
dcatProvider = lens _dcatProvider (\ s a -> s{_dcatProvider = a})

-- | The version of the custom action to delete.
dcatVersion :: Lens' DeleteCustomActionType Text
dcatVersion = lens _dcatVersion (\ s a -> s{_dcatVersion = a})

instance AWSRequest DeleteCustomActionType where
        type Rs DeleteCustomActionType =
             DeleteCustomActionTypeResponse
        request = postJSON codePipeline
        response
          = receiveNull DeleteCustomActionTypeResponse'

instance Hashable DeleteCustomActionType where

instance NFData DeleteCustomActionType where

instance ToHeaders DeleteCustomActionType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodePipeline_20150709.DeleteCustomActionType" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteCustomActionType where
        toJSON DeleteCustomActionType'{..}
          = object
              (catMaybes
                 [Just ("category" .= _dcatCategory),
                  Just ("provider" .= _dcatProvider),
                  Just ("version" .= _dcatVersion)])

instance ToPath DeleteCustomActionType where
        toPath = const "/"

instance ToQuery DeleteCustomActionType where
        toQuery = const mempty

-- | /See:/ 'deleteCustomActionTypeResponse' smart constructor.
data DeleteCustomActionTypeResponse =
  DeleteCustomActionTypeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCustomActionTypeResponse' with the minimum fields required to make a request.
--
deleteCustomActionTypeResponse
    :: DeleteCustomActionTypeResponse
deleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'


instance NFData DeleteCustomActionTypeResponse where
