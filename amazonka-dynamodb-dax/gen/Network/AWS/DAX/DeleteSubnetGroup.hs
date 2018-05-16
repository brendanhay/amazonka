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
-- Module      : Network.AWS.DAX.DeleteSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subnet group.
--
--
module Network.AWS.DAX.DeleteSubnetGroup
    (
    -- * Creating a Request
      deleteSubnetGroup
    , DeleteSubnetGroup
    -- * Request Lenses
    , dsgSubnetGroupName

    -- * Destructuring the Response
    , deleteSubnetGroupResponse
    , DeleteSubnetGroupResponse
    -- * Response Lenses
    , dsgrsDeletionMessage
    , dsgrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSubnetGroup' smart constructor.
newtype DeleteSubnetGroup = DeleteSubnetGroup'
  { _dsgSubnetGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgSubnetGroupName' - The name of the subnet group to delete.
deleteSubnetGroup
    :: Text -- ^ 'dsgSubnetGroupName'
    -> DeleteSubnetGroup
deleteSubnetGroup pSubnetGroupName_ =
  DeleteSubnetGroup' {_dsgSubnetGroupName = pSubnetGroupName_}


-- | The name of the subnet group to delete.
dsgSubnetGroupName :: Lens' DeleteSubnetGroup Text
dsgSubnetGroupName = lens _dsgSubnetGroupName (\ s a -> s{_dsgSubnetGroupName = a})

instance AWSRequest DeleteSubnetGroup where
        type Rs DeleteSubnetGroup = DeleteSubnetGroupResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSubnetGroupResponse' <$>
                   (x .?> "DeletionMessage") <*> (pure (fromEnum s)))

instance Hashable DeleteSubnetGroup where

instance NFData DeleteSubnetGroup where

instance ToHeaders DeleteSubnetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.DeleteSubnetGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSubnetGroup where
        toJSON DeleteSubnetGroup'{..}
          = object
              (catMaybes
                 [Just ("SubnetGroupName" .= _dsgSubnetGroupName)])

instance ToPath DeleteSubnetGroup where
        toPath = const "/"

instance ToQuery DeleteSubnetGroup where
        toQuery = const mempty

-- | /See:/ 'deleteSubnetGroupResponse' smart constructor.
data DeleteSubnetGroupResponse = DeleteSubnetGroupResponse'
  { _dsgrsDeletionMessage :: !(Maybe Text)
  , _dsgrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsgrsDeletionMessage' - A user-specified message for this action (i.e., a reason for deleting the subnet group).
--
-- * 'dsgrsResponseStatus' - -- | The response status code.
deleteSubnetGroupResponse
    :: Int -- ^ 'dsgrsResponseStatus'
    -> DeleteSubnetGroupResponse
deleteSubnetGroupResponse pResponseStatus_ =
  DeleteSubnetGroupResponse'
    {_dsgrsDeletionMessage = Nothing, _dsgrsResponseStatus = pResponseStatus_}


-- | A user-specified message for this action (i.e., a reason for deleting the subnet group).
dsgrsDeletionMessage :: Lens' DeleteSubnetGroupResponse (Maybe Text)
dsgrsDeletionMessage = lens _dsgrsDeletionMessage (\ s a -> s{_dsgrsDeletionMessage = a})

-- | -- | The response status code.
dsgrsResponseStatus :: Lens' DeleteSubnetGroupResponse Int
dsgrsResponseStatus = lens _dsgrsResponseStatus (\ s a -> s{_dsgrsResponseStatus = a})

instance NFData DeleteSubnetGroupResponse where
