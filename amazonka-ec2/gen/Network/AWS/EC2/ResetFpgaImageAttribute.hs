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
-- Module      : Network.AWS.EC2.ResetFpgaImageAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the specified attribute of the specified Amazon FPGA Image (AFI) to its default value. You can only reset the load permission attribute.
--
--
module Network.AWS.EC2.ResetFpgaImageAttribute
    (
    -- * Creating a Request
      resetFpgaImageAttribute
    , ResetFpgaImageAttribute
    -- * Request Lenses
    , rfiaAttribute
    , rfiaDryRun
    , rfiaFpgaImageId

    -- * Destructuring the Response
    , resetFpgaImageAttributeResponse
    , ResetFpgaImageAttributeResponse
    -- * Response Lenses
    , rfiarsReturn
    , rfiarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resetFpgaImageAttribute' smart constructor.
data ResetFpgaImageAttribute = ResetFpgaImageAttribute'
  { _rfiaAttribute   :: !(Maybe ResetFpgaImageAttributeName)
  , _rfiaDryRun      :: !(Maybe Bool)
  , _rfiaFpgaImageId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetFpgaImageAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfiaAttribute' - The attribute.
--
-- * 'rfiaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rfiaFpgaImageId' - The ID of the AFI.
resetFpgaImageAttribute
    :: Text -- ^ 'rfiaFpgaImageId'
    -> ResetFpgaImageAttribute
resetFpgaImageAttribute pFpgaImageId_ =
  ResetFpgaImageAttribute'
    { _rfiaAttribute = Nothing
    , _rfiaDryRun = Nothing
    , _rfiaFpgaImageId = pFpgaImageId_
    }


-- | The attribute.
rfiaAttribute :: Lens' ResetFpgaImageAttribute (Maybe ResetFpgaImageAttributeName)
rfiaAttribute = lens _rfiaAttribute (\ s a -> s{_rfiaAttribute = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rfiaDryRun :: Lens' ResetFpgaImageAttribute (Maybe Bool)
rfiaDryRun = lens _rfiaDryRun (\ s a -> s{_rfiaDryRun = a})

-- | The ID of the AFI.
rfiaFpgaImageId :: Lens' ResetFpgaImageAttribute Text
rfiaFpgaImageId = lens _rfiaFpgaImageId (\ s a -> s{_rfiaFpgaImageId = a})

instance AWSRequest ResetFpgaImageAttribute where
        type Rs ResetFpgaImageAttribute =
             ResetFpgaImageAttributeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ResetFpgaImageAttributeResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ResetFpgaImageAttribute where

instance NFData ResetFpgaImageAttribute where

instance ToHeaders ResetFpgaImageAttribute where
        toHeaders = const mempty

instance ToPath ResetFpgaImageAttribute where
        toPath = const "/"

instance ToQuery ResetFpgaImageAttribute where
        toQuery ResetFpgaImageAttribute'{..}
          = mconcat
              ["Action" =:
                 ("ResetFpgaImageAttribute" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Attribute" =: _rfiaAttribute,
               "DryRun" =: _rfiaDryRun,
               "FpgaImageId" =: _rfiaFpgaImageId]

-- | /See:/ 'resetFpgaImageAttributeResponse' smart constructor.
data ResetFpgaImageAttributeResponse = ResetFpgaImageAttributeResponse'
  { _rfiarsReturn         :: !(Maybe Bool)
  , _rfiarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetFpgaImageAttributeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rfiarsReturn' - Is @true@ if the request succeeds, and an error otherwise.
--
-- * 'rfiarsResponseStatus' - -- | The response status code.
resetFpgaImageAttributeResponse
    :: Int -- ^ 'rfiarsResponseStatus'
    -> ResetFpgaImageAttributeResponse
resetFpgaImageAttributeResponse pResponseStatus_ =
  ResetFpgaImageAttributeResponse'
    {_rfiarsReturn = Nothing, _rfiarsResponseStatus = pResponseStatus_}


-- | Is @true@ if the request succeeds, and an error otherwise.
rfiarsReturn :: Lens' ResetFpgaImageAttributeResponse (Maybe Bool)
rfiarsReturn = lens _rfiarsReturn (\ s a -> s{_rfiarsReturn = a})

-- | -- | The response status code.
rfiarsResponseStatus :: Lens' ResetFpgaImageAttributeResponse Int
rfiarsResponseStatus = lens _rfiarsResponseStatus (\ s a -> s{_rfiarsResponseStatus = a})

instance NFData ResetFpgaImageAttributeResponse where
