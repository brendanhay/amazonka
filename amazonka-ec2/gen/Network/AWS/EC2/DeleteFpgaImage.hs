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
-- Module      : Network.AWS.EC2.DeleteFpgaImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon FPGA Image (AFI).
--
--
module Network.AWS.EC2.DeleteFpgaImage
    (
    -- * Creating a Request
      deleteFpgaImage
    , DeleteFpgaImage
    -- * Request Lenses
    , dfiDryRun
    , dfiFpgaImageId

    -- * Destructuring the Response
    , deleteFpgaImageResponse
    , DeleteFpgaImageResponse
    -- * Response Lenses
    , delrsReturn
    , delrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFpgaImage' smart constructor.
data DeleteFpgaImage = DeleteFpgaImage'
  { _dfiDryRun      :: !(Maybe Bool)
  , _dfiFpgaImageId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFpgaImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfiFpgaImageId' - The ID of the AFI.
deleteFpgaImage
    :: Text -- ^ 'dfiFpgaImageId'
    -> DeleteFpgaImage
deleteFpgaImage pFpgaImageId_ =
  DeleteFpgaImage' {_dfiDryRun = Nothing, _dfiFpgaImageId = pFpgaImageId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfiDryRun :: Lens' DeleteFpgaImage (Maybe Bool)
dfiDryRun = lens _dfiDryRun (\ s a -> s{_dfiDryRun = a})

-- | The ID of the AFI.
dfiFpgaImageId :: Lens' DeleteFpgaImage Text
dfiFpgaImageId = lens _dfiFpgaImageId (\ s a -> s{_dfiFpgaImageId = a})

instance AWSRequest DeleteFpgaImage where
        type Rs DeleteFpgaImage = DeleteFpgaImageResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteFpgaImageResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable DeleteFpgaImage where

instance NFData DeleteFpgaImage where

instance ToHeaders DeleteFpgaImage where
        toHeaders = const mempty

instance ToPath DeleteFpgaImage where
        toPath = const "/"

instance ToQuery DeleteFpgaImage where
        toQuery DeleteFpgaImage'{..}
          = mconcat
              ["Action" =: ("DeleteFpgaImage" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dfiDryRun,
               "FpgaImageId" =: _dfiFpgaImageId]

-- | /See:/ 'deleteFpgaImageResponse' smart constructor.
data DeleteFpgaImageResponse = DeleteFpgaImageResponse'
  { _delrsReturn         :: !(Maybe Bool)
  , _delrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFpgaImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsReturn' - Is @true@ if the request succeeds, and an error otherwise.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteFpgaImageResponse
    :: Int -- ^ 'delrsResponseStatus'
    -> DeleteFpgaImageResponse
deleteFpgaImageResponse pResponseStatus_ =
  DeleteFpgaImageResponse'
    {_delrsReturn = Nothing, _delrsResponseStatus = pResponseStatus_}


-- | Is @true@ if the request succeeds, and an error otherwise.
delrsReturn :: Lens' DeleteFpgaImageResponse (Maybe Bool)
delrsReturn = lens _delrsReturn (\ s a -> s{_delrsReturn = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteFpgaImageResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\ s a -> s{_delrsResponseStatus = a})

instance NFData DeleteFpgaImageResponse where
