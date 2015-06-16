{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.ModifyHapg
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies an existing high-availability partition group.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyHapg.html>
module Network.AWS.CloudHSM.ModifyHapg
    (
    -- * Request
      ModifyHapg
    -- ** Request constructor
    , modifyHapg
    -- ** Request lenses
    , mhPartitionSerialList
    , mhLabel
    , mhHapgARN

    -- * Response
    , ModifyHapgResponse
    -- ** Response constructor
    , modifyHapgResponse
    -- ** Response lenses
    , mhrHapgARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'modifyHapg' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhPartitionSerialList'
--
-- * 'mhLabel'
--
-- * 'mhHapgARN'
data ModifyHapg = ModifyHapg'{_mhPartitionSerialList :: Maybe [Text], _mhLabel :: Maybe Text, _mhHapgARN :: Text} deriving (Eq, Read, Show)

-- | 'ModifyHapg' smart constructor.
modifyHapg :: Text -> ModifyHapg
modifyHapg pHapgARN = ModifyHapg'{_mhPartitionSerialList = Nothing, _mhLabel = Nothing, _mhHapgARN = pHapgARN};

-- | The list of partition serial numbers to make members of the
-- high-availability partition group.
mhPartitionSerialList :: Lens' ModifyHapg [Text]
mhPartitionSerialList = lens _mhPartitionSerialList (\ s a -> s{_mhPartitionSerialList = a}) . _Default;

-- | The new label for the high-availability partition group.
mhLabel :: Lens' ModifyHapg (Maybe Text)
mhLabel = lens _mhLabel (\ s a -> s{_mhLabel = a});

-- | The ARN of the high-availability partition group to modify.
mhHapgARN :: Lens' ModifyHapg Text
mhHapgARN = lens _mhHapgARN (\ s a -> s{_mhHapgARN = a});

instance AWSRequest ModifyHapg where
        type Sv ModifyHapg = CloudHSM
        type Rs ModifyHapg = ModifyHapgResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ModifyHapgResponse' <$> (x .?> "HapgArn"))

instance ToHeaders ModifyHapg where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ModifyHapg" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyHapg where
        toJSON ModifyHapg'{..}
          = object
              ["PartitionSerialList" .= _mhPartitionSerialList,
               "Label" .= _mhLabel, "HapgArn" .= _mhHapgARN]

instance ToPath ModifyHapg where
        toPath = const "/"

instance ToQuery ModifyHapg where
        toQuery = const mempty

-- | /See:/ 'modifyHapgResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhrHapgARN'
newtype ModifyHapgResponse = ModifyHapgResponse'{_mhrHapgARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ModifyHapgResponse' smart constructor.
modifyHapgResponse :: ModifyHapgResponse
modifyHapgResponse = ModifyHapgResponse'{_mhrHapgARN = Nothing};

-- | The ARN of the high-availability partition group.
mhrHapgARN :: Lens' ModifyHapgResponse (Maybe Text)
mhrHapgARN = lens _mhrHapgARN (\ s a -> s{_mhrHapgARN = a});
