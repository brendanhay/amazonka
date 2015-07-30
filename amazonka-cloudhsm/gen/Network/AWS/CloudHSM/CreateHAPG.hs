{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateHAPG
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a high-availability partition group. A high-availability
-- partition group is a group of partitions that spans multiple physical
-- HSMs.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateHAPG.html>
module Network.AWS.CloudHSM.CreateHAPG
    (
    -- * Request
      CreateHAPG
    -- ** Request constructor
    , createHAPG
    -- ** Request lenses
    , chLabel

    -- * Response
    , CreateHAPGResponse
    -- ** Response constructor
    , createHAPGResponse
    -- ** Response lenses
    , chrsHAPGARN
    , chrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateHapgRequest action.
--
-- /See:/ 'createHAPG' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chLabel'
newtype CreateHAPG = CreateHAPG'
    { _chLabel :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateHAPG' smart constructor.
createHAPG :: Text -> CreateHAPG
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
                    ("CloudHsmFrontendService.CreateHAPG" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHAPG where
        toJSON CreateHAPG'{..} = object ["Label" .= _chLabel]

instance ToPath CreateHAPG where
        toPath = const mempty

instance ToQuery CreateHAPG where
        toQuery = const mempty

-- | Contains the output of the CreateHAPartitionGroup action.
--
-- /See:/ 'createHAPGResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chrsHAPGARN'
--
-- * 'chrsStatus'
data CreateHAPGResponse = CreateHAPGResponse'
    { _chrsHAPGARN :: !(Maybe Text)
    , _chrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateHAPGResponse' smart constructor.
createHAPGResponse :: Int -> CreateHAPGResponse
createHAPGResponse pStatus_ =
    CreateHAPGResponse'
    { _chrsHAPGARN = Nothing
    , _chrsStatus = pStatus_
    }

-- | The ARN of the high-availability partition group.
chrsHAPGARN :: Lens' CreateHAPGResponse (Maybe Text)
chrsHAPGARN = lens _chrsHAPGARN (\ s a -> s{_chrsHAPGARN = a});

-- | FIXME: Undocumented member.
chrsStatus :: Lens' CreateHAPGResponse Int
chrsStatus = lens _chrsStatus (\ s a -> s{_chrsStatus = a});
