{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudHSM.ModifyHAPG
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
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyHAPG.html>
module Network.AWS.CloudHSM.ModifyHAPG
    (
    -- * Request
      ModifyHAPG
    -- ** Request constructor
    , modifyHAPG
    -- ** Request lenses
    , mhPartitionSerialList
    , mhLabel
    , mhHAPGARN

    -- * Response
    , ModifyHAPGResponse
    -- ** Response constructor
    , modifyHAPGResponse
    -- ** Response lenses
    , mhrHAPGARN
    , mhrStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyHAPG' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhPartitionSerialList'
--
-- * 'mhLabel'
--
-- * 'mhHAPGARN'
data ModifyHAPG = ModifyHAPG'
    { _mhPartitionSerialList :: !(Maybe [Text])
    , _mhLabel               :: !(Maybe Text)
    , _mhHAPGARN             :: !Text
    } deriving (Eq,Read,Show)

-- | 'ModifyHAPG' smart constructor.
modifyHAPG :: Text -> ModifyHAPG
modifyHAPG pHAPGARN =
    ModifyHAPG'
    { _mhPartitionSerialList = Nothing
    , _mhLabel = Nothing
    , _mhHAPGARN = pHAPGARN
    }

-- | The list of partition serial numbers to make members of the
-- high-availability partition group.
mhPartitionSerialList :: Lens' ModifyHAPG [Text]
mhPartitionSerialList = lens _mhPartitionSerialList (\ s a -> s{_mhPartitionSerialList = a}) . _Default;

-- | The new label for the high-availability partition group.
mhLabel :: Lens' ModifyHAPG (Maybe Text)
mhLabel = lens _mhLabel (\ s a -> s{_mhLabel = a});

-- | The ARN of the high-availability partition group to modify.
mhHAPGARN :: Lens' ModifyHAPG Text
mhHAPGARN = lens _mhHAPGARN (\ s a -> s{_mhHAPGARN = a});

instance AWSRequest ModifyHAPG where
        type Sv ModifyHAPG = CloudHSM
        type Rs ModifyHAPG = ModifyHAPGResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ModifyHAPGResponse' <$>
                   (x .?> "HapgArn") <*> (pure (fromEnum s)))

instance ToHeaders ModifyHAPG where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.ModifyHAPG" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyHAPG where
        toJSON ModifyHAPG'{..}
          = object
              ["PartitionSerialList" .= _mhPartitionSerialList,
               "Label" .= _mhLabel, "HapgArn" .= _mhHAPGARN]

instance ToPath ModifyHAPG where
        toPath = const "/"

instance ToQuery ModifyHAPG where
        toQuery = const mempty

-- | /See:/ 'modifyHAPGResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhrHAPGARN'
--
-- * 'mhrStatus'
data ModifyHAPGResponse = ModifyHAPGResponse'
    { _mhrHAPGARN :: !(Maybe Text)
    , _mhrStatus  :: !Int
    } deriving (Eq,Read,Show)

-- | 'ModifyHAPGResponse' smart constructor.
modifyHAPGResponse :: Int -> ModifyHAPGResponse
modifyHAPGResponse pStatus =
    ModifyHAPGResponse'
    { _mhrHAPGARN = Nothing
    , _mhrStatus = pStatus
    }

-- | The ARN of the high-availability partition group.
mhrHAPGARN :: Lens' ModifyHAPGResponse (Maybe Text)
mhrHAPGARN = lens _mhrHAPGARN (\ s a -> s{_mhrHAPGARN = a});

-- | FIXME: Undocumented member.
mhrStatus :: Lens' ModifyHAPGResponse Int
mhrStatus = lens _mhrStatus (\ s a -> s{_mhrStatus = a});
