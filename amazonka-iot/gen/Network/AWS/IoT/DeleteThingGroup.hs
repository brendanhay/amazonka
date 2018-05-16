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
-- Module      : Network.AWS.IoT.DeleteThingGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a thing group.
--
--
module Network.AWS.IoT.DeleteThingGroup
    (
    -- * Creating a Request
      deleteThingGroup
    , DeleteThingGroup
    -- * Request Lenses
    , dExpectedVersion
    , dThingGroupName

    -- * Destructuring the Response
    , deleteThingGroupResponse
    , DeleteThingGroupResponse
    -- * Response Lenses
    , dtgtrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteThingGroup' smart constructor.
data DeleteThingGroup = DeleteThingGroup'
  { _dExpectedVersion :: !(Maybe Integer)
  , _dThingGroupName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteThingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dExpectedVersion' - The expected version of the thing group to delete.
--
-- * 'dThingGroupName' - The name of the thing group to delete.
deleteThingGroup
    :: Text -- ^ 'dThingGroupName'
    -> DeleteThingGroup
deleteThingGroup pThingGroupName_ =
  DeleteThingGroup'
    {_dExpectedVersion = Nothing, _dThingGroupName = pThingGroupName_}


-- | The expected version of the thing group to delete.
dExpectedVersion :: Lens' DeleteThingGroup (Maybe Integer)
dExpectedVersion = lens _dExpectedVersion (\ s a -> s{_dExpectedVersion = a})

-- | The name of the thing group to delete.
dThingGroupName :: Lens' DeleteThingGroup Text
dThingGroupName = lens _dThingGroupName (\ s a -> s{_dThingGroupName = a})

instance AWSRequest DeleteThingGroup where
        type Rs DeleteThingGroup = DeleteThingGroupResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteThingGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteThingGroup where

instance NFData DeleteThingGroup where

instance ToHeaders DeleteThingGroup where
        toHeaders = const mempty

instance ToPath DeleteThingGroup where
        toPath DeleteThingGroup'{..}
          = mconcat ["/thing-groups/", toBS _dThingGroupName]

instance ToQuery DeleteThingGroup where
        toQuery DeleteThingGroup'{..}
          = mconcat ["expectedVersion" =: _dExpectedVersion]

-- | /See:/ 'deleteThingGroupResponse' smart constructor.
newtype DeleteThingGroupResponse = DeleteThingGroupResponse'
  { _dtgtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteThingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgtrsResponseStatus' - -- | The response status code.
deleteThingGroupResponse
    :: Int -- ^ 'dtgtrsResponseStatus'
    -> DeleteThingGroupResponse
deleteThingGroupResponse pResponseStatus_ =
  DeleteThingGroupResponse' {_dtgtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dtgtrsResponseStatus :: Lens' DeleteThingGroupResponse Int
dtgtrsResponseStatus = lens _dtgtrsResponseStatus (\ s a -> s{_dtgtrsResponseStatus = a})

instance NFData DeleteThingGroupResponse where
