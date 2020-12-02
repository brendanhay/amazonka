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
-- Module      : Network.AWS.EC2.CopyFpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified Amazon FPGA Image (AFI) to the current Region.
module Network.AWS.EC2.CopyFpgaImage
  ( -- * Creating a Request
    copyFpgaImage,
    CopyFpgaImage,

    -- * Request Lenses
    cfiClientToken,
    cfiName,
    cfiDescription,
    cfiDryRun,
    cfiSourceFpgaImageId,
    cfiSourceRegion,

    -- * Destructuring the Response
    copyFpgaImageResponse,
    CopyFpgaImageResponse,

    -- * Response Lenses
    coprsFpgaImageId,
    coprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'copyFpgaImage' smart constructor.
data CopyFpgaImage = CopyFpgaImage'
  { _cfiClientToken ::
      !(Maybe Text),
    _cfiName :: !(Maybe Text),
    _cfiDescription :: !(Maybe Text),
    _cfiDryRun :: !(Maybe Bool),
    _cfiSourceFpgaImageId :: !Text,
    _cfiSourceRegion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyFpgaImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfiClientToken' - Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- * 'cfiName' - The name for the new AFI. The default is the name of the source AFI.
--
-- * 'cfiDescription' - The description for the new AFI.
--
-- * 'cfiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cfiSourceFpgaImageId' - The ID of the source AFI.
--
-- * 'cfiSourceRegion' - The Region that contains the source AFI.
copyFpgaImage ::
  -- | 'cfiSourceFpgaImageId'
  Text ->
  -- | 'cfiSourceRegion'
  Text ->
  CopyFpgaImage
copyFpgaImage pSourceFpgaImageId_ pSourceRegion_ =
  CopyFpgaImage'
    { _cfiClientToken = Nothing,
      _cfiName = Nothing,
      _cfiDescription = Nothing,
      _cfiDryRun = Nothing,
      _cfiSourceFpgaImageId = pSourceFpgaImageId_,
      _cfiSourceRegion = pSourceRegion_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency> .
cfiClientToken :: Lens' CopyFpgaImage (Maybe Text)
cfiClientToken = lens _cfiClientToken (\s a -> s {_cfiClientToken = a})

-- | The name for the new AFI. The default is the name of the source AFI.
cfiName :: Lens' CopyFpgaImage (Maybe Text)
cfiName = lens _cfiName (\s a -> s {_cfiName = a})

-- | The description for the new AFI.
cfiDescription :: Lens' CopyFpgaImage (Maybe Text)
cfiDescription = lens _cfiDescription (\s a -> s {_cfiDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cfiDryRun :: Lens' CopyFpgaImage (Maybe Bool)
cfiDryRun = lens _cfiDryRun (\s a -> s {_cfiDryRun = a})

-- | The ID of the source AFI.
cfiSourceFpgaImageId :: Lens' CopyFpgaImage Text
cfiSourceFpgaImageId = lens _cfiSourceFpgaImageId (\s a -> s {_cfiSourceFpgaImageId = a})

-- | The Region that contains the source AFI.
cfiSourceRegion :: Lens' CopyFpgaImage Text
cfiSourceRegion = lens _cfiSourceRegion (\s a -> s {_cfiSourceRegion = a})

instance AWSRequest CopyFpgaImage where
  type Rs CopyFpgaImage = CopyFpgaImageResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CopyFpgaImageResponse'
            <$> (x .@? "fpgaImageId") <*> (pure (fromEnum s))
      )

instance Hashable CopyFpgaImage

instance NFData CopyFpgaImage

instance ToHeaders CopyFpgaImage where
  toHeaders = const mempty

instance ToPath CopyFpgaImage where
  toPath = const "/"

instance ToQuery CopyFpgaImage where
  toQuery CopyFpgaImage' {..} =
    mconcat
      [ "Action" =: ("CopyFpgaImage" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _cfiClientToken,
        "Name" =: _cfiName,
        "Description" =: _cfiDescription,
        "DryRun" =: _cfiDryRun,
        "SourceFpgaImageId" =: _cfiSourceFpgaImageId,
        "SourceRegion" =: _cfiSourceRegion
      ]

-- | /See:/ 'copyFpgaImageResponse' smart constructor.
data CopyFpgaImageResponse = CopyFpgaImageResponse'
  { _coprsFpgaImageId ::
      !(Maybe Text),
    _coprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyFpgaImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coprsFpgaImageId' - The ID of the new AFI.
--
-- * 'coprsResponseStatus' - -- | The response status code.
copyFpgaImageResponse ::
  -- | 'coprsResponseStatus'
  Int ->
  CopyFpgaImageResponse
copyFpgaImageResponse pResponseStatus_ =
  CopyFpgaImageResponse'
    { _coprsFpgaImageId = Nothing,
      _coprsResponseStatus = pResponseStatus_
    }

-- | The ID of the new AFI.
coprsFpgaImageId :: Lens' CopyFpgaImageResponse (Maybe Text)
coprsFpgaImageId = lens _coprsFpgaImageId (\s a -> s {_coprsFpgaImageId = a})

-- | -- | The response status code.
coprsResponseStatus :: Lens' CopyFpgaImageResponse Int
coprsResponseStatus = lens _coprsResponseStatus (\s a -> s {_coprsResponseStatus = a})

instance NFData CopyFpgaImageResponse
