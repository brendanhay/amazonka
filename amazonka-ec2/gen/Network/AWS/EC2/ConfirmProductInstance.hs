{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ConfirmProductInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Determines whether a product code is associated with an instance. This
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
    , cpirqDryRun
    , cpirqProductCode
    , cpirqInstanceId

    -- * Response
    , ConfirmProductInstanceResponse
    -- ** Response constructor
    , confirmProductInstanceResponse
    -- ** Response lenses
    , cpirsReturn
    , cpirsOwnerId
    , cpirsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'confirmProductInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpirqDryRun'
--
-- * 'cpirqProductCode'
--
-- * 'cpirqInstanceId'
data ConfirmProductInstance = ConfirmProductInstance'
    { _cpirqDryRun      :: !(Maybe Bool)
    , _cpirqProductCode :: !Text
    , _cpirqInstanceId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmProductInstance' smart constructor.
confirmProductInstance :: Text -> Text -> ConfirmProductInstance
confirmProductInstance pProductCode_ pInstanceId_ =
    ConfirmProductInstance'
    { _cpirqDryRun = Nothing
    , _cpirqProductCode = pProductCode_
    , _cpirqInstanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cpirqDryRun :: Lens' ConfirmProductInstance (Maybe Bool)
cpirqDryRun = lens _cpirqDryRun (\ s a -> s{_cpirqDryRun = a});

-- | The product code. This must be a product code that you own.
cpirqProductCode :: Lens' ConfirmProductInstance Text
cpirqProductCode = lens _cpirqProductCode (\ s a -> s{_cpirqProductCode = a});

-- | The ID of the instance.
cpirqInstanceId :: Lens' ConfirmProductInstance Text
cpirqInstanceId = lens _cpirqInstanceId (\ s a -> s{_cpirqInstanceId = a});

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
               "DryRun" =: _cpirqDryRun,
               "ProductCode" =: _cpirqProductCode,
               "InstanceId" =: _cpirqInstanceId]

-- | /See:/ 'confirmProductInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpirsReturn'
--
-- * 'cpirsOwnerId'
--
-- * 'cpirsStatus'
data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse'
    { _cpirsReturn  :: !(Maybe Bool)
    , _cpirsOwnerId :: !(Maybe Text)
    , _cpirsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmProductInstanceResponse' smart constructor.
confirmProductInstanceResponse :: Int -> ConfirmProductInstanceResponse
confirmProductInstanceResponse pStatus_ =
    ConfirmProductInstanceResponse'
    { _cpirsReturn = Nothing
    , _cpirsOwnerId = Nothing
    , _cpirsStatus = pStatus_
    }

-- | The return value of the request. Returns @true@ if the specified product
-- code is owned by the requester and associated with the specified
-- instance.
cpirsReturn :: Lens' ConfirmProductInstanceResponse (Maybe Bool)
cpirsReturn = lens _cpirsReturn (\ s a -> s{_cpirsReturn = a});

-- | The AWS account ID of the instance owner. This is only present if the
-- product code is attached to the instance.
cpirsOwnerId :: Lens' ConfirmProductInstanceResponse (Maybe Text)
cpirsOwnerId = lens _cpirsOwnerId (\ s a -> s{_cpirsOwnerId = a});

-- | FIXME: Undocumented member.
cpirsStatus :: Lens' ConfirmProductInstanceResponse Int
cpirsStatus = lens _cpirsStatus (\ s a -> s{_cpirsStatus = a});
