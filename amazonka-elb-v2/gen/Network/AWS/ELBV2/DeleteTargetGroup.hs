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
-- Module      : Network.AWS.ELBV2.DeleteTargetGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified target group.
--
-- You can delete a target group if it is not referenced by any actions. Deleting a target group also deletes any associated health checks.
module Network.AWS.ELBV2.DeleteTargetGroup
    (
    -- * Creating a Request
      deleteTargetGroup
    , DeleteTargetGroup
    -- * Request Lenses
    , dtgTargetGroupARN

    -- * Destructuring the Response
    , deleteTargetGroupResponse
    , DeleteTargetGroupResponse
    -- * Response Lenses
    , dtgrsResponseStatus
    ) where

import           Network.AWS.ELBV2.Types
import           Network.AWS.ELBV2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DeleteTargetGroup.
--
-- /See:/ 'deleteTargetGroup' smart constructor.
newtype DeleteTargetGroup = DeleteTargetGroup'
    { _dtgTargetGroupARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgTargetGroupARN'
deleteTargetGroup
    :: Text -- ^ 'dtgTargetGroupARN'
    -> DeleteTargetGroup
deleteTargetGroup pTargetGroupARN_ =
    DeleteTargetGroup'
    { _dtgTargetGroupARN = pTargetGroupARN_
    }

-- | The Amazon Resource Name (ARN) of the target group.
dtgTargetGroupARN :: Lens' DeleteTargetGroup Text
dtgTargetGroupARN = lens _dtgTargetGroupARN (\ s a -> s{_dtgTargetGroupARN = a});

instance AWSRequest DeleteTargetGroup where
        type Rs DeleteTargetGroup = DeleteTargetGroupResponse
        request = postQuery elbv2
        response
          = receiveXMLWrapper "DeleteTargetGroupResult"
              (\ s h x ->
                 DeleteTargetGroupResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTargetGroup

instance NFData DeleteTargetGroup

instance ToHeaders DeleteTargetGroup where
        toHeaders = const mempty

instance ToPath DeleteTargetGroup where
        toPath = const "/"

instance ToQuery DeleteTargetGroup where
        toQuery DeleteTargetGroup'{..}
          = mconcat
              ["Action" =: ("DeleteTargetGroup" :: ByteString),
               "Version" =: ("2015-12-01" :: ByteString),
               "TargetGroupArn" =: _dtgTargetGroupARN]

-- | Contains the output of DeleteTargetGroup.
--
-- /See:/ 'deleteTargetGroupResponse' smart constructor.
newtype DeleteTargetGroupResponse = DeleteTargetGroupResponse'
    { _dtgrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTargetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgrsResponseStatus'
deleteTargetGroupResponse
    :: Int -- ^ 'dtgrsResponseStatus'
    -> DeleteTargetGroupResponse
deleteTargetGroupResponse pResponseStatus_ =
    DeleteTargetGroupResponse'
    { _dtgrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
dtgrsResponseStatus :: Lens' DeleteTargetGroupResponse Int
dtgrsResponseStatus = lens _dtgrsResponseStatus (\ s a -> s{_dtgrsResponseStatus = a});

instance NFData DeleteTargetGroupResponse
