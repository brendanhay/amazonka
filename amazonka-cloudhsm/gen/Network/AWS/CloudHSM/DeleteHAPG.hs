{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.DeleteHAPG
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a high-availability partition group.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteHAPG.html>
module Network.AWS.CloudHSM.DeleteHAPG
    (
    -- * Request
      DeleteHAPG
    -- ** Request constructor
    , deleteHAPG
    -- ** Request lenses
    , dhrqHAPGARN

    -- * Response
    , DeleteHAPGResponse
    -- ** Response constructor
    , deleteHAPGResponse
    -- ** Response lenses
    , dhrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DeleteHapg action.
--
-- /See:/ 'deleteHAPG' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhrqHAPGARN'
newtype DeleteHAPG = DeleteHAPG'
    { _dhrqHAPGARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteHAPG' smart constructor.
deleteHAPG :: Text -> DeleteHAPG
deleteHAPG pHAPGARN =
    DeleteHAPG'
    { _dhrqHAPGARN = pHAPGARN
    }

-- | The ARN of the high-availability partition group to delete.
dhrqHAPGARN :: Lens' DeleteHAPG Text
dhrqHAPGARN = lens _dhrqHAPGARN (\ s a -> s{_dhrqHAPGARN = a});

instance AWSRequest DeleteHAPG where
        type Sv DeleteHAPG = CloudHSM
        type Rs DeleteHAPG = DeleteHAPGResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteHAPGResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteHAPG where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DeleteHAPG" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteHAPG where
        toJSON DeleteHAPG'{..}
          = object ["HapgArn" .= _dhrqHAPGARN]

instance ToPath DeleteHAPG where
        toPath = const "/"

instance ToQuery DeleteHAPG where
        toQuery = const mempty

-- | Contains the output of the DeleteHapg action.
--
-- /See:/ 'deleteHAPGResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhrsStatus'
newtype DeleteHAPGResponse = DeleteHAPGResponse'
    { _dhrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteHAPGResponse' smart constructor.
deleteHAPGResponse :: Int -> DeleteHAPGResponse
deleteHAPGResponse pStatus =
    DeleteHAPGResponse'
    { _dhrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
dhrsStatus :: Lens' DeleteHAPGResponse Int
dhrsStatus = lens _dhrsStatus (\ s a -> s{_dhrsStatus = a});
