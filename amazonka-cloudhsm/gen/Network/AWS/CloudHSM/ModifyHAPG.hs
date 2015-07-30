{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.ModifyHAPG
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing high-availability partition group.
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
    , mhrsHAPGARN
    , mhrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyHAPG' smart constructor.
modifyHAPG :: Text -> ModifyHAPG
modifyHAPG pHAPGARN_ =
    ModifyHAPG'
    { _mhPartitionSerialList = Nothing
    , _mhLabel = Nothing
    , _mhHAPGARN = pHAPGARN_
    }

-- | The list of partition serial numbers to make members of the
-- high-availability partition group.
mhPartitionSerialList :: Lens' ModifyHAPG [Text]
mhPartitionSerialList = lens _mhPartitionSerialList (\ s a -> s{_mhPartitionSerialList = a}) . _Default . _Coerce;

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
        toPath = const mempty

instance ToQuery ModifyHAPG where
        toQuery = const mempty

-- | /See:/ 'modifyHAPGResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhrsHAPGARN'
--
-- * 'mhrsStatus'
data ModifyHAPGResponse = ModifyHAPGResponse'
    { _mhrsHAPGARN :: !(Maybe Text)
    , _mhrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyHAPGResponse' smart constructor.
modifyHAPGResponse :: Int -> ModifyHAPGResponse
modifyHAPGResponse pStatus_ =
    ModifyHAPGResponse'
    { _mhrsHAPGARN = Nothing
    , _mhrsStatus = pStatus_
    }

-- | The ARN of the high-availability partition group.
mhrsHAPGARN :: Lens' ModifyHAPGResponse (Maybe Text)
mhrsHAPGARN = lens _mhrsHAPGARN (\ s a -> s{_mhrsHAPGARN = a});

-- | FIXME: Undocumented member.
mhrsStatus :: Lens' ModifyHAPGResponse Int
mhrsStatus = lens _mhrsStatus (\ s a -> s{_mhrsStatus = a});
