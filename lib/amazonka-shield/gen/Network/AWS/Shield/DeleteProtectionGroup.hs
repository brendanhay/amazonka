{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DeleteProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified protection group.
module Network.AWS.Shield.DeleteProtectionGroup
  ( -- * Creating a Request
    deleteProtectionGroup,
    DeleteProtectionGroup,

    -- * Request Lenses
    dpgProtectionGroupId,

    -- * Destructuring the Response
    deleteProtectionGroupResponse,
    DeleteProtectionGroupResponse,

    -- * Response Lenses
    dpgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Shield.Types

-- | /See:/ 'deleteProtectionGroup' smart constructor.
newtype DeleteProtectionGroup = DeleteProtectionGroup'
  { _dpgProtectionGroupId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProtectionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgProtectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
deleteProtectionGroup ::
  -- | 'dpgProtectionGroupId'
  Text ->
  DeleteProtectionGroup
deleteProtectionGroup pProtectionGroupId_ =
  DeleteProtectionGroup'
    { _dpgProtectionGroupId =
        pProtectionGroupId_
    }

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
dpgProtectionGroupId :: Lens' DeleteProtectionGroup Text
dpgProtectionGroupId = lens _dpgProtectionGroupId (\s a -> s {_dpgProtectionGroupId = a})

instance AWSRequest DeleteProtectionGroup where
  type Rs DeleteProtectionGroup = DeleteProtectionGroupResponse
  request = postJSON shield
  response =
    receiveEmpty
      (\s h x -> DeleteProtectionGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteProtectionGroup

instance NFData DeleteProtectionGroup

instance ToHeaders DeleteProtectionGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSShield_20160616.DeleteProtectionGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteProtectionGroup where
  toJSON DeleteProtectionGroup' {..} =
    object
      (catMaybes [Just ("ProtectionGroupId" .= _dpgProtectionGroupId)])

instance ToPath DeleteProtectionGroup where
  toPath = const "/"

instance ToQuery DeleteProtectionGroup where
  toQuery = const mempty

-- | /See:/ 'deleteProtectionGroupResponse' smart constructor.
newtype DeleteProtectionGroupResponse = DeleteProtectionGroupResponse'
  { _dpgrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProtectionGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgrsResponseStatus' - -- | The response status code.
deleteProtectionGroupResponse ::
  -- | 'dpgrsResponseStatus'
  Int ->
  DeleteProtectionGroupResponse
deleteProtectionGroupResponse pResponseStatus_ =
  DeleteProtectionGroupResponse'
    { _dpgrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dpgrsResponseStatus :: Lens' DeleteProtectionGroupResponse Int
dpgrsResponseStatus = lens _dpgrsResponseStatus (\s a -> s {_dpgrsResponseStatus = a})

instance NFData DeleteProtectionGroupResponse
