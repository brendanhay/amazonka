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
-- Module      : Network.AWS.EC2.DeleteFpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon FPGA Image (AFI).
module Network.AWS.EC2.DeleteFpgaImage
  ( -- * Creating a Request
    deleteFpgaImage,
    DeleteFpgaImage,

    -- * Request Lenses
    dfiDryRun,
    dfiFpgaImageId,

    -- * Destructuring the Response
    deleteFpgaImageResponse,
    DeleteFpgaImageResponse,

    -- * Response Lenses
    dfifrsReturn,
    dfifrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFpgaImage' smart constructor.
data DeleteFpgaImage = DeleteFpgaImage'
  { _dfiDryRun ::
      !(Maybe Bool),
    _dfiFpgaImageId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFpgaImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfiFpgaImageId' - The ID of the AFI.
deleteFpgaImage ::
  -- | 'dfiFpgaImageId'
  Text ->
  DeleteFpgaImage
deleteFpgaImage pFpgaImageId_ =
  DeleteFpgaImage'
    { _dfiDryRun = Nothing,
      _dfiFpgaImageId = pFpgaImageId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfiDryRun :: Lens' DeleteFpgaImage (Maybe Bool)
dfiDryRun = lens _dfiDryRun (\s a -> s {_dfiDryRun = a})

-- | The ID of the AFI.
dfiFpgaImageId :: Lens' DeleteFpgaImage Text
dfiFpgaImageId = lens _dfiFpgaImageId (\s a -> s {_dfiFpgaImageId = a})

instance AWSRequest DeleteFpgaImage where
  type Rs DeleteFpgaImage = DeleteFpgaImageResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteFpgaImageResponse'
            <$> (x .@? "return") <*> (pure (fromEnum s))
      )

instance Hashable DeleteFpgaImage

instance NFData DeleteFpgaImage

instance ToHeaders DeleteFpgaImage where
  toHeaders = const mempty

instance ToPath DeleteFpgaImage where
  toPath = const "/"

instance ToQuery DeleteFpgaImage where
  toQuery DeleteFpgaImage' {..} =
    mconcat
      [ "Action" =: ("DeleteFpgaImage" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dfiDryRun,
        "FpgaImageId" =: _dfiFpgaImageId
      ]

-- | /See:/ 'deleteFpgaImageResponse' smart constructor.
data DeleteFpgaImageResponse = DeleteFpgaImageResponse'
  { _dfifrsReturn ::
      !(Maybe Bool),
    _dfifrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFpgaImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfifrsReturn' - Is @true@ if the request succeeds, and an error otherwise.
--
-- * 'dfifrsResponseStatus' - -- | The response status code.
deleteFpgaImageResponse ::
  -- | 'dfifrsResponseStatus'
  Int ->
  DeleteFpgaImageResponse
deleteFpgaImageResponse pResponseStatus_ =
  DeleteFpgaImageResponse'
    { _dfifrsReturn = Nothing,
      _dfifrsResponseStatus = pResponseStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
dfifrsReturn :: Lens' DeleteFpgaImageResponse (Maybe Bool)
dfifrsReturn = lens _dfifrsReturn (\s a -> s {_dfifrsReturn = a})

-- | -- | The response status code.
dfifrsResponseStatus :: Lens' DeleteFpgaImageResponse Int
dfifrsResponseStatus = lens _dfifrsResponseStatus (\s a -> s {_dfifrsResponseStatus = a})

instance NFData DeleteFpgaImageResponse
