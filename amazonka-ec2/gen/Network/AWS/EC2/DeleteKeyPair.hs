{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteKeyPair
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified key pair, by removing the public key from Amazon
-- EC2.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteKeyPair.html>
module Network.AWS.EC2.DeleteKeyPair
    (
    -- * Request
      DeleteKeyPair
    -- ** Request constructor
    , deleteKeyPair
    -- ** Request lenses
    , dkprqDryRun
    , dkprqKeyName

    -- * Response
    , DeleteKeyPairResponse
    -- ** Response constructor
    , deleteKeyPairResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteKeyPair' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dkprqDryRun'
--
-- * 'dkprqKeyName'
data DeleteKeyPair = DeleteKeyPair'
    { _dkprqDryRun  :: !(Maybe Bool)
    , _dkprqKeyName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteKeyPair' smart constructor.
deleteKeyPair :: Text -> DeleteKeyPair
deleteKeyPair pKeyName_ =
    DeleteKeyPair'
    { _dkprqDryRun = Nothing
    , _dkprqKeyName = pKeyName_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dkprqDryRun :: Lens' DeleteKeyPair (Maybe Bool)
dkprqDryRun = lens _dkprqDryRun (\ s a -> s{_dkprqDryRun = a});

-- | The name of the key pair.
dkprqKeyName :: Lens' DeleteKeyPair Text
dkprqKeyName = lens _dkprqKeyName (\ s a -> s{_dkprqKeyName = a});

instance AWSRequest DeleteKeyPair where
        type Sv DeleteKeyPair = EC2
        type Rs DeleteKeyPair = DeleteKeyPairResponse
        request = post
        response = receiveNull DeleteKeyPairResponse'

instance ToHeaders DeleteKeyPair where
        toHeaders = const mempty

instance ToPath DeleteKeyPair where
        toPath = const "/"

instance ToQuery DeleteKeyPair where
        toQuery DeleteKeyPair'{..}
          = mconcat
              ["Action" =: ("DeleteKeyPair" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dkprqDryRun, "KeyName" =: _dkprqKeyName]

-- | /See:/ 'deleteKeyPairResponse' smart constructor.
data DeleteKeyPairResponse =
    DeleteKeyPairResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteKeyPairResponse' smart constructor.
deleteKeyPairResponse :: DeleteKeyPairResponse
deleteKeyPairResponse = DeleteKeyPairResponse'
