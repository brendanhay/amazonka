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
-- Module      : Network.AWS.CodeStar.DeleteProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a project, including project resources. Does not delete users associated with the project, but does delete the IAM roles that allowed access to the project.
--
--
module Network.AWS.CodeStar.DeleteProject
    (
    -- * Creating a Request
      deleteProject
    , DeleteProject
    -- * Request Lenses
    , dpDeleteStack
    , dpClientRequestToken
    , dpId

    -- * Destructuring the Response
    , deleteProjectResponse
    , DeleteProjectResponse
    -- * Response Lenses
    , dprsProjectARN
    , dprsStackId
    , dprsResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { _dpDeleteStack        :: !(Maybe Bool)
  , _dpClientRequestToken :: !(Maybe Text)
  , _dpId                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpDeleteStack' - Whether to send a delete request for the primary stack in AWS CloudFormation originally used to generate the project and its resources. This option will delete all AWS resources for the project (except for any buckets in Amazon S3) as well as deleting the project itself. Recommended for most use cases.
--
-- * 'dpClientRequestToken' - A user- or system-generated token that identifies the entity that requested project deletion. This token can be used to repeat the request.
--
-- * 'dpId' - The ID of the project to be deleted in AWS CodeStar.
deleteProject
    :: Text -- ^ 'dpId'
    -> DeleteProject
deleteProject pId_ =
  DeleteProject'
    {_dpDeleteStack = Nothing, _dpClientRequestToken = Nothing, _dpId = pId_}


-- | Whether to send a delete request for the primary stack in AWS CloudFormation originally used to generate the project and its resources. This option will delete all AWS resources for the project (except for any buckets in Amazon S3) as well as deleting the project itself. Recommended for most use cases.
dpDeleteStack :: Lens' DeleteProject (Maybe Bool)
dpDeleteStack = lens _dpDeleteStack (\ s a -> s{_dpDeleteStack = a})

-- | A user- or system-generated token that identifies the entity that requested project deletion. This token can be used to repeat the request.
dpClientRequestToken :: Lens' DeleteProject (Maybe Text)
dpClientRequestToken = lens _dpClientRequestToken (\ s a -> s{_dpClientRequestToken = a})

-- | The ID of the project to be deleted in AWS CodeStar.
dpId :: Lens' DeleteProject Text
dpId = lens _dpId (\ s a -> s{_dpId = a})

instance AWSRequest DeleteProject where
        type Rs DeleteProject = DeleteProjectResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 DeleteProjectResponse' <$>
                   (x .?> "projectArn") <*> (x .?> "stackId") <*>
                     (pure (fromEnum s)))

instance Hashable DeleteProject where

instance NFData DeleteProject where

instance ToHeaders DeleteProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.DeleteProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProject where
        toJSON DeleteProject'{..}
          = object
              (catMaybes
                 [("deleteStack" .=) <$> _dpDeleteStack,
                  ("clientRequestToken" .=) <$> _dpClientRequestToken,
                  Just ("id" .= _dpId)])

instance ToPath DeleteProject where
        toPath = const "/"

instance ToQuery DeleteProject where
        toQuery = const mempty

-- | /See:/ 'deleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { _dprsProjectARN     :: !(Maybe Text)
  , _dprsStackId        :: !(Maybe Text)
  , _dprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsProjectARN' - The Amazon Resource Name (ARN) of the deleted project.
--
-- * 'dprsStackId' - The ID of the primary stack in AWS CloudFormation that will be deleted as part of deleting the project and its resources.
--
-- * 'dprsResponseStatus' - -- | The response status code.
deleteProjectResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DeleteProjectResponse
deleteProjectResponse pResponseStatus_ =
  DeleteProjectResponse'
    { _dprsProjectARN = Nothing
    , _dprsStackId = Nothing
    , _dprsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the deleted project.
dprsProjectARN :: Lens' DeleteProjectResponse (Maybe Text)
dprsProjectARN = lens _dprsProjectARN (\ s a -> s{_dprsProjectARN = a})

-- | The ID of the primary stack in AWS CloudFormation that will be deleted as part of deleting the project and its resources.
dprsStackId :: Lens' DeleteProjectResponse (Maybe Text)
dprsStackId = lens _dprsStackId (\ s a -> s{_dprsStackId = a})

-- | -- | The response status code.
dprsResponseStatus :: Lens' DeleteProjectResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DeleteProjectResponse where
