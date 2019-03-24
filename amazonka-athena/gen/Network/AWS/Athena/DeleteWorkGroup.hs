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
-- Module      : Network.AWS.Athena.DeleteWorkGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the workgroup with the specified name. The primary workgroup cannot be deleted.
--
--
module Network.AWS.Athena.DeleteWorkGroup
    (
    -- * Creating a Request
      deleteWorkGroup
    , DeleteWorkGroup
    -- * Request Lenses
    , dwgRecursiveDeleteOption
    , dwgWorkGroup

    -- * Destructuring the Response
    , deleteWorkGroupResponse
    , DeleteWorkGroupResponse
    -- * Response Lenses
    , dwgrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteWorkGroup' smart constructor.
data DeleteWorkGroup = DeleteWorkGroup'
  { _dwgRecursiveDeleteOption :: !(Maybe Bool)
  , _dwgWorkGroup             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWorkGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwgRecursiveDeleteOption' - The option to delete the workgroup and its contents even if the workgroup contains any named queries.
--
-- * 'dwgWorkGroup' - The unique name of the workgroup to delete.
deleteWorkGroup
    :: Text -- ^ 'dwgWorkGroup'
    -> DeleteWorkGroup
deleteWorkGroup pWorkGroup_ =
  DeleteWorkGroup'
    {_dwgRecursiveDeleteOption = Nothing, _dwgWorkGroup = pWorkGroup_}


-- | The option to delete the workgroup and its contents even if the workgroup contains any named queries.
dwgRecursiveDeleteOption :: Lens' DeleteWorkGroup (Maybe Bool)
dwgRecursiveDeleteOption = lens _dwgRecursiveDeleteOption (\ s a -> s{_dwgRecursiveDeleteOption = a})

-- | The unique name of the workgroup to delete.
dwgWorkGroup :: Lens' DeleteWorkGroup Text
dwgWorkGroup = lens _dwgWorkGroup (\ s a -> s{_dwgWorkGroup = a})

instance AWSRequest DeleteWorkGroup where
        type Rs DeleteWorkGroup = DeleteWorkGroupResponse
        request = postJSON athena
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteWorkGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteWorkGroup where

instance NFData DeleteWorkGroup where

instance ToHeaders DeleteWorkGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.DeleteWorkGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteWorkGroup where
        toJSON DeleteWorkGroup'{..}
          = object
              (catMaybes
                 [("RecursiveDeleteOption" .=) <$>
                    _dwgRecursiveDeleteOption,
                  Just ("WorkGroup" .= _dwgWorkGroup)])

instance ToPath DeleteWorkGroup where
        toPath = const "/"

instance ToQuery DeleteWorkGroup where
        toQuery = const mempty

-- | /See:/ 'deleteWorkGroupResponse' smart constructor.
newtype DeleteWorkGroupResponse = DeleteWorkGroupResponse'
  { _dwgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteWorkGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwgrsResponseStatus' - -- | The response status code.
deleteWorkGroupResponse
    :: Int -- ^ 'dwgrsResponseStatus'
    -> DeleteWorkGroupResponse
deleteWorkGroupResponse pResponseStatus_ =
  DeleteWorkGroupResponse' {_dwgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dwgrsResponseStatus :: Lens' DeleteWorkGroupResponse Int
dwgrsResponseStatus = lens _dwgrsResponseStatus (\ s a -> s{_dwgrsResponseStatus = a})

instance NFData DeleteWorkGroupResponse where
