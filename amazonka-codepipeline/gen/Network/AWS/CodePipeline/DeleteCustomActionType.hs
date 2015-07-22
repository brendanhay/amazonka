{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.DeleteCustomActionType
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Marks a custom action as deleted. PollForJobs for the custom action will
-- fail after the action is marked for deletion. Only used for custom
-- actions.
--
-- You cannot recreate a custom action after it has been deleted unless you
-- increase the version number of the action.
--
-- <http://docs.aws.amazon.com/codepipeline/latest/APIReference/API_DeleteCustomActionType.html>
module Network.AWS.CodePipeline.DeleteCustomActionType
    (
    -- * Request
      DeleteCustomActionType
    -- ** Request constructor
    , deleteCustomActionType
    -- ** Request lenses
    , dcatrqCategory
    , dcatrqProvider
    , dcatrqVersion

    -- * Response
    , DeleteCustomActionTypeResponse
    -- ** Response constructor
    , deleteCustomActionTypeResponse
    ) where

import           Network.AWS.CodePipeline.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a delete custom action operation. The custom
-- action will be marked as deleted.
--
-- /See:/ 'deleteCustomActionType' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcatrqCategory'
--
-- * 'dcatrqProvider'
--
-- * 'dcatrqVersion'
data DeleteCustomActionType = DeleteCustomActionType'
    { _dcatrqCategory :: !ActionCategory
    , _dcatrqProvider :: !Text
    , _dcatrqVersion  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCustomActionType' smart constructor.
deleteCustomActionType :: ActionCategory -> Text -> Text -> DeleteCustomActionType
deleteCustomActionType pCategory_ pProvider_ pVersion_ =
    DeleteCustomActionType'
    { _dcatrqCategory = pCategory_
    , _dcatrqProvider = pProvider_
    , _dcatrqVersion = pVersion_
    }

-- | The category of the custom action that you want to delete, such as
-- source or deploy.
dcatrqCategory :: Lens' DeleteCustomActionType ActionCategory
dcatrqCategory = lens _dcatrqCategory (\ s a -> s{_dcatrqCategory = a});

-- | The provider of the service used in the custom action, such as AWS
-- CodeDeploy.
dcatrqProvider :: Lens' DeleteCustomActionType Text
dcatrqProvider = lens _dcatrqProvider (\ s a -> s{_dcatrqProvider = a});

-- | The version of the custom action to delete.
dcatrqVersion :: Lens' DeleteCustomActionType Text
dcatrqVersion = lens _dcatrqVersion (\ s a -> s{_dcatrqVersion = a});

instance AWSRequest DeleteCustomActionType where
        type Sv DeleteCustomActionType = CodePipeline
        type Rs DeleteCustomActionType =
             DeleteCustomActionTypeResponse
        request = postJSON
        response
          = receiveNull DeleteCustomActionTypeResponse'

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
              ["category" .= _dcatrqCategory,
               "provider" .= _dcatrqProvider,
               "version" .= _dcatrqVersion]

instance ToPath DeleteCustomActionType where
        toPath = const "/"

instance ToQuery DeleteCustomActionType where
        toQuery = const mempty

-- | /See:/ 'deleteCustomActionTypeResponse' smart constructor.
data DeleteCustomActionTypeResponse =
    DeleteCustomActionTypeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCustomActionTypeResponse' smart constructor.
deleteCustomActionTypeResponse :: DeleteCustomActionTypeResponse
deleteCustomActionTypeResponse = DeleteCustomActionTypeResponse'
