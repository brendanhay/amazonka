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
-- Module      : Network.AWS.CloudHSM.CreateHAPG
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a high-availability partition group. A high-availability
-- partition group is a group of partitions that spans multiple physical
-- HSMs.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateHAPG.html AWS API Reference> for CreateHAPG.
module Network.AWS.CloudHSM.CreateHAPG
    (
    -- * Creating a Request
      createHAPG
    , CreateHAPG
    -- * Request Lenses
    , chLabel

    -- * Destructuring the Response
    , createHAPGResponse
    , CreateHAPGResponse
    -- * Response Lenses
    , chrsHAPGARN
    , chrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateHapgRequest action.
--
-- /See:/ 'createHAPG' smart constructor.
newtype CreateHAPG = CreateHAPG'
    { _chLabel :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHAPG' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chLabel'
createHAPG
    :: Text -- ^ 'chLabel'
    -> CreateHAPG
createHAPG pLabel_ =
    CreateHAPG'
    { _chLabel = pLabel_
    }

-- | The label of the new high-availability partition group.
chLabel :: Lens' CreateHAPG Text
chLabel = lens _chLabel (\ s a -> s{_chLabel = a});

instance AWSRequest CreateHAPG where
        type Sv CreateHAPG = CloudHSM
        type Rs CreateHAPG = CreateHAPGResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateHAPGResponse' <$>
                   (x .?> "HapgArn") <*> (pure (fromEnum s)))

instance ToHeaders CreateHAPG where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.CreateHapg" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHAPG where
        toJSON CreateHAPG'{..}
          = object (catMaybes [Just ("Label" .= _chLabel)])

instance ToPath CreateHAPG where
        toPath = const "/"

instance ToQuery CreateHAPG where
        toQuery = const mempty

-- | Contains the output of the CreateHAPartitionGroup action.
--
-- /See:/ 'createHAPGResponse' smart constructor.
data CreateHAPGResponse = CreateHAPGResponse'
    { _chrsHAPGARN :: !(Maybe Text)
    , _chrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHAPGResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chrsHAPGARN'
--
-- * 'chrsStatus'
createHAPGResponse
    :: Int -- ^ 'chrsStatus'
    -> CreateHAPGResponse
createHAPGResponse pStatus_ =
    CreateHAPGResponse'
    { _chrsHAPGARN = Nothing
    , _chrsStatus = pStatus_
    }

-- | The ARN of the high-availability partition group.
chrsHAPGARN :: Lens' CreateHAPGResponse (Maybe Text)
chrsHAPGARN = lens _chrsHAPGARN (\ s a -> s{_chrsHAPGARN = a});

-- | The response status code.
chrsStatus :: Lens' CreateHAPGResponse Int
chrsStatus = lens _chrsStatus (\ s a -> s{_chrsStatus = a});
