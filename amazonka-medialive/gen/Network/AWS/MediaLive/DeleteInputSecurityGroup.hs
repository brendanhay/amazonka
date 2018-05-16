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
-- Module      : Network.AWS.MediaLive.DeleteInputSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Input Security Group
module Network.AWS.MediaLive.DeleteInputSecurityGroup
    (
    -- * Creating a Request
      deleteInputSecurityGroup
    , DeleteInputSecurityGroup
    -- * Request Lenses
    , dInputSecurityGroupId

    -- * Destructuring the Response
    , deleteInputSecurityGroupResponse
    , DeleteInputSecurityGroupResponse
    -- * Response Lenses
    , disgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteInputSecurityGroupRequest
--
-- /See:/ 'deleteInputSecurityGroup' smart constructor.
newtype DeleteInputSecurityGroup = DeleteInputSecurityGroup'
  { _dInputSecurityGroupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInputSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dInputSecurityGroupId' - The Input Security Group to delete
deleteInputSecurityGroup
    :: Text -- ^ 'dInputSecurityGroupId'
    -> DeleteInputSecurityGroup
deleteInputSecurityGroup pInputSecurityGroupId_ =
  DeleteInputSecurityGroup' {_dInputSecurityGroupId = pInputSecurityGroupId_}


-- | The Input Security Group to delete
dInputSecurityGroupId :: Lens' DeleteInputSecurityGroup Text
dInputSecurityGroupId = lens _dInputSecurityGroupId (\ s a -> s{_dInputSecurityGroupId = a})

instance AWSRequest DeleteInputSecurityGroup where
        type Rs DeleteInputSecurityGroup =
             DeleteInputSecurityGroupResponse
        request = delete mediaLive
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteInputSecurityGroupResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteInputSecurityGroup where

instance NFData DeleteInputSecurityGroup where

instance ToHeaders DeleteInputSecurityGroup where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteInputSecurityGroup where
        toPath DeleteInputSecurityGroup'{..}
          = mconcat
              ["/prod/inputSecurityGroups/",
               toBS _dInputSecurityGroupId]

instance ToQuery DeleteInputSecurityGroup where
        toQuery = const mempty

-- | Placeholder documentation for DeleteInputSecurityGroupResponse
--
-- /See:/ 'deleteInputSecurityGroupResponse' smart constructor.
newtype DeleteInputSecurityGroupResponse = DeleteInputSecurityGroupResponse'
  { _disgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInputSecurityGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disgrsResponseStatus' - -- | The response status code.
deleteInputSecurityGroupResponse
    :: Int -- ^ 'disgrsResponseStatus'
    -> DeleteInputSecurityGroupResponse
deleteInputSecurityGroupResponse pResponseStatus_ =
  DeleteInputSecurityGroupResponse' {_disgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
disgrsResponseStatus :: Lens' DeleteInputSecurityGroupResponse Int
disgrsResponseStatus = lens _disgrsResponseStatus (\ s a -> s{_disgrsResponseStatus = a})

instance NFData DeleteInputSecurityGroupResponse
         where
