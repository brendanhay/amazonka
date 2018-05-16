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
-- Module      : Network.AWS.DAX.DeleteParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified parameter group. You cannot delete a parameter group if it is associated with any DAX clusters.
--
--
module Network.AWS.DAX.DeleteParameterGroup
    (
    -- * Creating a Request
      deleteParameterGroup
    , DeleteParameterGroup
    -- * Request Lenses
    , dpgParameterGroupName

    -- * Destructuring the Response
    , deleteParameterGroupResponse
    , DeleteParameterGroupResponse
    -- * Response Lenses
    , dpgrsDeletionMessage
    , dpgrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteParameterGroup' smart constructor.
newtype DeleteParameterGroup = DeleteParameterGroup'
  { _dpgParameterGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgParameterGroupName' - The name of the parameter group to delete.
deleteParameterGroup
    :: Text -- ^ 'dpgParameterGroupName'
    -> DeleteParameterGroup
deleteParameterGroup pParameterGroupName_ =
  DeleteParameterGroup' {_dpgParameterGroupName = pParameterGroupName_}


-- | The name of the parameter group to delete.
dpgParameterGroupName :: Lens' DeleteParameterGroup Text
dpgParameterGroupName = lens _dpgParameterGroupName (\ s a -> s{_dpgParameterGroupName = a})

instance AWSRequest DeleteParameterGroup where
        type Rs DeleteParameterGroup =
             DeleteParameterGroupResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 DeleteParameterGroupResponse' <$>
                   (x .?> "DeletionMessage") <*> (pure (fromEnum s)))

instance Hashable DeleteParameterGroup where

instance NFData DeleteParameterGroup where

instance ToHeaders DeleteParameterGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.DeleteParameterGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteParameterGroup where
        toJSON DeleteParameterGroup'{..}
          = object
              (catMaybes
                 [Just
                    ("ParameterGroupName" .= _dpgParameterGroupName)])

instance ToPath DeleteParameterGroup where
        toPath = const "/"

instance ToQuery DeleteParameterGroup where
        toQuery = const mempty

-- | /See:/ 'deleteParameterGroupResponse' smart constructor.
data DeleteParameterGroupResponse = DeleteParameterGroupResponse'
  { _dpgrsDeletionMessage :: !(Maybe Text)
  , _dpgrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteParameterGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgrsDeletionMessage' - A user-specified message for this action (i.e., a reason for deleting the parameter group).
--
-- * 'dpgrsResponseStatus' - -- | The response status code.
deleteParameterGroupResponse
    :: Int -- ^ 'dpgrsResponseStatus'
    -> DeleteParameterGroupResponse
deleteParameterGroupResponse pResponseStatus_ =
  DeleteParameterGroupResponse'
    {_dpgrsDeletionMessage = Nothing, _dpgrsResponseStatus = pResponseStatus_}


-- | A user-specified message for this action (i.e., a reason for deleting the parameter group).
dpgrsDeletionMessage :: Lens' DeleteParameterGroupResponse (Maybe Text)
dpgrsDeletionMessage = lens _dpgrsDeletionMessage (\ s a -> s{_dpgrsDeletionMessage = a})

-- | -- | The response status code.
dpgrsResponseStatus :: Lens' DeleteParameterGroupResponse Int
dpgrsResponseStatus = lens _dpgrsResponseStatus (\ s a -> s{_dpgrsResponseStatus = a})

instance NFData DeleteParameterGroupResponse where
