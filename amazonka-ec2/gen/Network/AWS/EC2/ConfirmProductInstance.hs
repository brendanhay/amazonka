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
-- Module      : Network.AWS.EC2.ConfirmProductInstance
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines whether a product code is associated with an instance. This action can only be used by the owner of the product code. It is useful when a product code owner must verify whether another user's instance is eligible for support.
--
--
module Network.AWS.EC2.ConfirmProductInstance
    (
    -- * Creating a Request
      confirmProductInstance
    , ConfirmProductInstance
    -- * Request Lenses
    , cpiDryRun
    , cpiInstanceId
    , cpiProductCode

    -- * Destructuring the Response
    , confirmProductInstanceResponse
    , ConfirmProductInstanceResponse
    -- * Response Lenses
    , cpirsReturn
    , cpirsOwnerId
    , cpirsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'confirmProductInstance' smart constructor.
data ConfirmProductInstance = ConfirmProductInstance'
  { _cpiDryRun      :: !(Maybe Bool)
  , _cpiInstanceId  :: !Text
  , _cpiProductCode :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmProductInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cpiInstanceId' - The ID of the instance.
--
-- * 'cpiProductCode' - The product code. This must be a product code that you own.
confirmProductInstance
    :: Text -- ^ 'cpiInstanceId'
    -> Text -- ^ 'cpiProductCode'
    -> ConfirmProductInstance
confirmProductInstance pInstanceId_ pProductCode_ =
  ConfirmProductInstance'
    { _cpiDryRun = Nothing
    , _cpiInstanceId = pInstanceId_
    , _cpiProductCode = pProductCode_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cpiDryRun :: Lens' ConfirmProductInstance (Maybe Bool)
cpiDryRun = lens _cpiDryRun (\ s a -> s{_cpiDryRun = a})

-- | The ID of the instance.
cpiInstanceId :: Lens' ConfirmProductInstance Text
cpiInstanceId = lens _cpiInstanceId (\ s a -> s{_cpiInstanceId = a})

-- | The product code. This must be a product code that you own.
cpiProductCode :: Lens' ConfirmProductInstance Text
cpiProductCode = lens _cpiProductCode (\ s a -> s{_cpiProductCode = a})

instance AWSRequest ConfirmProductInstance where
        type Rs ConfirmProductInstance =
             ConfirmProductInstanceResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ConfirmProductInstanceResponse' <$>
                   (x .@? "return") <*> (x .@? "ownerId") <*>
                     (pure (fromEnum s)))

instance Hashable ConfirmProductInstance where

instance NFData ConfirmProductInstance where

instance ToHeaders ConfirmProductInstance where
        toHeaders = const mempty

instance ToPath ConfirmProductInstance where
        toPath = const "/"

instance ToQuery ConfirmProductInstance where
        toQuery ConfirmProductInstance'{..}
          = mconcat
              ["Action" =:
                 ("ConfirmProductInstance" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _cpiDryRun,
               "InstanceId" =: _cpiInstanceId,
               "ProductCode" =: _cpiProductCode]

-- | /See:/ 'confirmProductInstanceResponse' smart constructor.
data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse'
  { _cpirsReturn         :: !(Maybe Bool)
  , _cpirsOwnerId        :: !(Maybe Text)
  , _cpirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmProductInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpirsReturn' - The return value of the request. Returns @true@ if the specified product code is owned by the requester and associated with the specified instance.
--
-- * 'cpirsOwnerId' - The AWS account ID of the instance owner. This is only present if the product code is attached to the instance.
--
-- * 'cpirsResponseStatus' - -- | The response status code.
confirmProductInstanceResponse
    :: Int -- ^ 'cpirsResponseStatus'
    -> ConfirmProductInstanceResponse
confirmProductInstanceResponse pResponseStatus_ =
  ConfirmProductInstanceResponse'
    { _cpirsReturn = Nothing
    , _cpirsOwnerId = Nothing
    , _cpirsResponseStatus = pResponseStatus_
    }


-- | The return value of the request. Returns @true@ if the specified product code is owned by the requester and associated with the specified instance.
cpirsReturn :: Lens' ConfirmProductInstanceResponse (Maybe Bool)
cpirsReturn = lens _cpirsReturn (\ s a -> s{_cpirsReturn = a})

-- | The AWS account ID of the instance owner. This is only present if the product code is attached to the instance.
cpirsOwnerId :: Lens' ConfirmProductInstanceResponse (Maybe Text)
cpirsOwnerId = lens _cpirsOwnerId (\ s a -> s{_cpirsOwnerId = a})

-- | -- | The response status code.
cpirsResponseStatus :: Lens' ConfirmProductInstanceResponse Int
cpirsResponseStatus = lens _cpirsResponseStatus (\ s a -> s{_cpirsResponseStatus = a})

instance NFData ConfirmProductInstanceResponse where
