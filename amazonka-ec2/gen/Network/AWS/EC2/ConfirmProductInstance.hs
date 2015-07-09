{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.ConfirmProductInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Determines whether a product code is associated with an instance. This
-- action can only be used by the owner of the product code. It is useful
-- when a product code owner needs to verify whether another user\'s
-- instance is eligible for support.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ConfirmProductInstance.html>
module Network.AWS.EC2.ConfirmProductInstance
    (
    -- * Request
      ConfirmProductInstance
    -- ** Request constructor
    , confirmProductInstance
    -- ** Request lenses
    , cpiDryRun
    , cpiProductCode
    , cpiInstanceId

    -- * Response
    , ConfirmProductInstanceResponse
    -- ** Response constructor
    , confirmProductInstanceResponse
    -- ** Response lenses
    , cpirReturn
    , cpirOwnerId
    , cpirStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'confirmProductInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpiDryRun'
--
-- * 'cpiProductCode'
--
-- * 'cpiInstanceId'
data ConfirmProductInstance = ConfirmProductInstance'
    { _cpiDryRun      :: !(Maybe Bool)
    , _cpiProductCode :: !Text
    , _cpiInstanceId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmProductInstance' smart constructor.
confirmProductInstance :: Text -> Text -> ConfirmProductInstance
confirmProductInstance pProductCode pInstanceId =
    ConfirmProductInstance'
    { _cpiDryRun = Nothing
    , _cpiProductCode = pProductCode
    , _cpiInstanceId = pInstanceId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cpiDryRun :: Lens' ConfirmProductInstance (Maybe Bool)
cpiDryRun = lens _cpiDryRun (\ s a -> s{_cpiDryRun = a});

-- | The product code. This must be a product code that you own.
cpiProductCode :: Lens' ConfirmProductInstance Text
cpiProductCode = lens _cpiProductCode (\ s a -> s{_cpiProductCode = a});

-- | The ID of the instance.
cpiInstanceId :: Lens' ConfirmProductInstance Text
cpiInstanceId = lens _cpiInstanceId (\ s a -> s{_cpiInstanceId = a});

instance AWSRequest ConfirmProductInstance where
        type Sv ConfirmProductInstance = EC2
        type Rs ConfirmProductInstance =
             ConfirmProductInstanceResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ConfirmProductInstanceResponse' <$>
                   (x .@? "return") <*> (x .@? "ownerId") <*>
                     (pure (fromEnum s)))

instance ToHeaders ConfirmProductInstance where
        toHeaders = const mempty

instance ToPath ConfirmProductInstance where
        toPath = const "/"

instance ToQuery ConfirmProductInstance where
        toQuery ConfirmProductInstance'{..}
          = mconcat
              ["Action" =:
                 ("ConfirmProductInstance" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cpiDryRun,
               "ProductCode" =: _cpiProductCode,
               "InstanceId" =: _cpiInstanceId]

-- | /See:/ 'confirmProductInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpirReturn'
--
-- * 'cpirOwnerId'
--
-- * 'cpirStatus'
data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse'
    { _cpirReturn  :: !(Maybe Bool)
    , _cpirOwnerId :: !(Maybe Text)
    , _cpirStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmProductInstanceResponse' smart constructor.
confirmProductInstanceResponse :: Int -> ConfirmProductInstanceResponse
confirmProductInstanceResponse pStatus =
    ConfirmProductInstanceResponse'
    { _cpirReturn = Nothing
    , _cpirOwnerId = Nothing
    , _cpirStatus = pStatus
    }

-- | The return value of the request. Returns @true@ if the specified product
-- code is owned by the requester and associated with the specified
-- instance.
cpirReturn :: Lens' ConfirmProductInstanceResponse (Maybe Bool)
cpirReturn = lens _cpirReturn (\ s a -> s{_cpirReturn = a});

-- | The AWS account ID of the instance owner. This is only present if the
-- product code is attached to the instance.
cpirOwnerId :: Lens' ConfirmProductInstanceResponse (Maybe Text)
cpirOwnerId = lens _cpirOwnerId (\ s a -> s{_cpirOwnerId = a});

-- | FIXME: Undocumented member.
cpirStatus :: Lens' ConfirmProductInstanceResponse Int
cpirStatus = lens _cpirStatus (\ s a -> s{_cpirStatus = a});
