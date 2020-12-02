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
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror target.
--
--
-- You cannot delete a Traffic Mirror target that is in use by a Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorTarget
  ( -- * Creating a Request
    deleteTrafficMirrorTarget,
    DeleteTrafficMirrorTarget,

    -- * Request Lenses
    dtmttDryRun,
    dtmttTrafficMirrorTargetId,

    -- * Destructuring the Response
    deleteTrafficMirrorTargetResponse,
    DeleteTrafficMirrorTargetResponse,

    -- * Response Lenses
    dtmttrsTrafficMirrorTargetId,
    dtmttrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTrafficMirrorTarget' smart constructor.
data DeleteTrafficMirrorTarget = DeleteTrafficMirrorTarget'
  { _dtmttDryRun ::
      !(Maybe Bool),
    _dtmttTrafficMirrorTargetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTrafficMirrorTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmttDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtmttTrafficMirrorTargetId' - The ID of the Traffic Mirror target.
deleteTrafficMirrorTarget ::
  -- | 'dtmttTrafficMirrorTargetId'
  Text ->
  DeleteTrafficMirrorTarget
deleteTrafficMirrorTarget pTrafficMirrorTargetId_ =
  DeleteTrafficMirrorTarget'
    { _dtmttDryRun = Nothing,
      _dtmttTrafficMirrorTargetId = pTrafficMirrorTargetId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtmttDryRun :: Lens' DeleteTrafficMirrorTarget (Maybe Bool)
dtmttDryRun = lens _dtmttDryRun (\s a -> s {_dtmttDryRun = a})

-- | The ID of the Traffic Mirror target.
dtmttTrafficMirrorTargetId :: Lens' DeleteTrafficMirrorTarget Text
dtmttTrafficMirrorTargetId = lens _dtmttTrafficMirrorTargetId (\s a -> s {_dtmttTrafficMirrorTargetId = a})

instance AWSRequest DeleteTrafficMirrorTarget where
  type
    Rs DeleteTrafficMirrorTarget =
      DeleteTrafficMirrorTargetResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteTrafficMirrorTargetResponse'
            <$> (x .@? "trafficMirrorTargetId") <*> (pure (fromEnum s))
      )

instance Hashable DeleteTrafficMirrorTarget

instance NFData DeleteTrafficMirrorTarget

instance ToHeaders DeleteTrafficMirrorTarget where
  toHeaders = const mempty

instance ToPath DeleteTrafficMirrorTarget where
  toPath = const "/"

instance ToQuery DeleteTrafficMirrorTarget where
  toQuery DeleteTrafficMirrorTarget' {..} =
    mconcat
      [ "Action" =: ("DeleteTrafficMirrorTarget" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dtmttDryRun,
        "TrafficMirrorTargetId" =: _dtmttTrafficMirrorTargetId
      ]

-- | /See:/ 'deleteTrafficMirrorTargetResponse' smart constructor.
data DeleteTrafficMirrorTargetResponse = DeleteTrafficMirrorTargetResponse'
  { _dtmttrsTrafficMirrorTargetId ::
      !(Maybe Text),
    _dtmttrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTrafficMirrorTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmttrsTrafficMirrorTargetId' - The ID of the deleted Traffic Mirror target.
--
-- * 'dtmttrsResponseStatus' - -- | The response status code.
deleteTrafficMirrorTargetResponse ::
  -- | 'dtmttrsResponseStatus'
  Int ->
  DeleteTrafficMirrorTargetResponse
deleteTrafficMirrorTargetResponse pResponseStatus_ =
  DeleteTrafficMirrorTargetResponse'
    { _dtmttrsTrafficMirrorTargetId =
        Nothing,
      _dtmttrsResponseStatus = pResponseStatus_
    }

-- | The ID of the deleted Traffic Mirror target.
dtmttrsTrafficMirrorTargetId :: Lens' DeleteTrafficMirrorTargetResponse (Maybe Text)
dtmttrsTrafficMirrorTargetId = lens _dtmttrsTrafficMirrorTargetId (\s a -> s {_dtmttrsTrafficMirrorTargetId = a})

-- | -- | The response status code.
dtmttrsResponseStatus :: Lens' DeleteTrafficMirrorTargetResponse Int
dtmttrsResponseStatus = lens _dtmttrsResponseStatus (\s a -> s {_dtmttrsResponseStatus = a})

instance NFData DeleteTrafficMirrorTargetResponse
