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
    , mhrqPartitionSerialList
    , mhrqLabel
    , mhrqHAPGARN

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
-- * 'mhrqPartitionSerialList'
--
-- * 'mhrqLabel'
--
-- * 'mhrqHAPGARN'
data ModifyHAPG = ModifyHAPG'
    { _mhrqPartitionSerialList :: !(Maybe [Text])
    , _mhrqLabel               :: !(Maybe Text)
    , _mhrqHAPGARN             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyHAPG' smart constructor.
modifyHAPG :: Text -> ModifyHAPG
modifyHAPG pHAPGARN_ =
    ModifyHAPG'
    { _mhrqPartitionSerialList = Nothing
    , _mhrqLabel = Nothing
    , _mhrqHAPGARN = pHAPGARN_
    }

-- | The list of partition serial numbers to make members of the
-- high-availability partition group.
mhrqPartitionSerialList :: Lens' ModifyHAPG [Text]
mhrqPartitionSerialList = lens _mhrqPartitionSerialList (\ s a -> s{_mhrqPartitionSerialList = a}) . _Default;

-- | The new label for the high-availability partition group.
mhrqLabel :: Lens' ModifyHAPG (Maybe Text)
mhrqLabel = lens _mhrqLabel (\ s a -> s{_mhrqLabel = a});

-- | The ARN of the high-availability partition group to modify.
mhrqHAPGARN :: Lens' ModifyHAPG Text
mhrqHAPGARN = lens _mhrqHAPGARN (\ s a -> s{_mhrqHAPGARN = a});

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
              ["PartitionSerialList" .= _mhrqPartitionSerialList,
               "Label" .= _mhrqLabel, "HapgArn" .= _mhrqHAPGARN]

instance ToPath ModifyHAPG where
        toPath = const "/"

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
