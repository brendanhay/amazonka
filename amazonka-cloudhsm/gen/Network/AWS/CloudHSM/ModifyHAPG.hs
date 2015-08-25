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
-- Module      : Network.AWS.CloudHSM.ModifyHAPG
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing high-availability partition group.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyHAPG.html AWS API Reference> for ModifyHAPG.
module Network.AWS.CloudHSM.ModifyHAPG
    (
    -- * Creating a Request
      modifyHAPG
    , ModifyHAPG
    -- * Request Lenses
    , mhPartitionSerialList
    , mhLabel
    , mhHAPGARN

    -- * Destructuring the Response
    , modifyHAPGResponse
    , ModifyHAPGResponse
    -- * Response Lenses
    , mhrsHAPGARN
    , mhrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyHAPG' smart constructor.
data ModifyHAPG = ModifyHAPG'
    { _mhPartitionSerialList :: !(Maybe [Text])
    , _mhLabel               :: !(Maybe Text)
    , _mhHAPGARN             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyHAPG' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhPartitionSerialList'
--
-- * 'mhLabel'
--
-- * 'mhHAPGARN'
modifyHAPG
    :: Text -- ^ 'mhHAPGARN'
    -> ModifyHAPG
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
        type Rs ModifyHAPG = ModifyHAPGResponse
        request = postJSON cloudHSM
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
                    ("CloudHsmFrontendService.ModifyHapg" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyHAPG where
        toJSON ModifyHAPG'{..}
          = object
              (catMaybes
                 [("PartitionSerialList" .=) <$>
                    _mhPartitionSerialList,
                  ("Label" .=) <$> _mhLabel,
                  Just ("HapgArn" .= _mhHAPGARN)])

instance ToPath ModifyHAPG where
        toPath = const "/"

instance ToQuery ModifyHAPG where
        toQuery = const mempty

-- | /See:/ 'modifyHAPGResponse' smart constructor.
data ModifyHAPGResponse = ModifyHAPGResponse'
    { _mhrsHAPGARN :: !(Maybe Text)
    , _mhrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyHAPGResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mhrsHAPGARN'
--
-- * 'mhrsStatus'
modifyHAPGResponse
    :: Int -- ^ 'mhrsStatus'
    -> ModifyHAPGResponse
modifyHAPGResponse pStatus_ =
    ModifyHAPGResponse'
    { _mhrsHAPGARN = Nothing
    , _mhrsStatus = pStatus_
    }

-- | The ARN of the high-availability partition group.
mhrsHAPGARN :: Lens' ModifyHAPGResponse (Maybe Text)
mhrsHAPGARN = lens _mhrsHAPGARN (\ s a -> s{_mhrsHAPGARN = a});

-- | The response status code.
mhrsStatus :: Lens' ModifyHAPGResponse Int
mhrsStatus = lens _mhrsStatus (\ s a -> s{_mhrsStatus = a});
