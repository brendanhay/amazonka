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
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror filter.
--
--
-- You cannot delete a Traffic Mirror filter that is in use by a Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorFilter
  ( -- * Creating a Request
    deleteTrafficMirrorFilter,
    DeleteTrafficMirrorFilter,

    -- * Request Lenses
    dtmftDryRun,
    dtmftTrafficMirrorFilterId,

    -- * Destructuring the Response
    deleteTrafficMirrorFilterResponse,
    DeleteTrafficMirrorFilterResponse,

    -- * Response Lenses
    dtmfrsTrafficMirrorFilterId,
    dtmfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTrafficMirrorFilter' smart constructor.
data DeleteTrafficMirrorFilter = DeleteTrafficMirrorFilter'
  { _dtmftDryRun ::
      !(Maybe Bool),
    _dtmftTrafficMirrorFilterId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTrafficMirrorFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmftDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtmftTrafficMirrorFilterId' - The ID of the Traffic Mirror filter.
deleteTrafficMirrorFilter ::
  -- | 'dtmftTrafficMirrorFilterId'
  Text ->
  DeleteTrafficMirrorFilter
deleteTrafficMirrorFilter pTrafficMirrorFilterId_ =
  DeleteTrafficMirrorFilter'
    { _dtmftDryRun = Nothing,
      _dtmftTrafficMirrorFilterId = pTrafficMirrorFilterId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtmftDryRun :: Lens' DeleteTrafficMirrorFilter (Maybe Bool)
dtmftDryRun = lens _dtmftDryRun (\s a -> s {_dtmftDryRun = a})

-- | The ID of the Traffic Mirror filter.
dtmftTrafficMirrorFilterId :: Lens' DeleteTrafficMirrorFilter Text
dtmftTrafficMirrorFilterId = lens _dtmftTrafficMirrorFilterId (\s a -> s {_dtmftTrafficMirrorFilterId = a})

instance AWSRequest DeleteTrafficMirrorFilter where
  type
    Rs DeleteTrafficMirrorFilter =
      DeleteTrafficMirrorFilterResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterResponse'
            <$> (x .@? "trafficMirrorFilterId") <*> (pure (fromEnum s))
      )

instance Hashable DeleteTrafficMirrorFilter

instance NFData DeleteTrafficMirrorFilter

instance ToHeaders DeleteTrafficMirrorFilter where
  toHeaders = const mempty

instance ToPath DeleteTrafficMirrorFilter where
  toPath = const "/"

instance ToQuery DeleteTrafficMirrorFilter where
  toQuery DeleteTrafficMirrorFilter' {..} =
    mconcat
      [ "Action" =: ("DeleteTrafficMirrorFilter" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dtmftDryRun,
        "TrafficMirrorFilterId" =: _dtmftTrafficMirrorFilterId
      ]

-- | /See:/ 'deleteTrafficMirrorFilterResponse' smart constructor.
data DeleteTrafficMirrorFilterResponse = DeleteTrafficMirrorFilterResponse'
  { _dtmfrsTrafficMirrorFilterId ::
      !(Maybe Text),
    _dtmfrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTrafficMirrorFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmfrsTrafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- * 'dtmfrsResponseStatus' - -- | The response status code.
deleteTrafficMirrorFilterResponse ::
  -- | 'dtmfrsResponseStatus'
  Int ->
  DeleteTrafficMirrorFilterResponse
deleteTrafficMirrorFilterResponse pResponseStatus_ =
  DeleteTrafficMirrorFilterResponse'
    { _dtmfrsTrafficMirrorFilterId =
        Nothing,
      _dtmfrsResponseStatus = pResponseStatus_
    }

-- | The ID of the Traffic Mirror filter.
dtmfrsTrafficMirrorFilterId :: Lens' DeleteTrafficMirrorFilterResponse (Maybe Text)
dtmfrsTrafficMirrorFilterId = lens _dtmfrsTrafficMirrorFilterId (\s a -> s {_dtmfrsTrafficMirrorFilterId = a})

-- | -- | The response status code.
dtmfrsResponseStatus :: Lens' DeleteTrafficMirrorFilterResponse Int
dtmfrsResponseStatus = lens _dtmfrsResponseStatus (\s a -> s {_dtmfrsResponseStatus = a})

instance NFData DeleteTrafficMirrorFilterResponse
