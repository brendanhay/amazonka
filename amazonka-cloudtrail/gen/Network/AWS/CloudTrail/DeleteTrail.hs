{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.DeleteTrail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a trail.
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_DeleteTrail.html>
module Network.AWS.CloudTrail.DeleteTrail
    (
    -- * Request
      DeleteTrail
    -- ** Request constructor
    , deleteTrail
    -- ** Request lenses
    , dtName

    -- * Response
    , DeleteTrailResponse
    -- ** Response constructor
    , deleteTrailResponse
    -- ** Response lenses
    , drsStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request that specifies the name of a trail to delete.
--
-- /See:/ 'deleteTrail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtName'
newtype DeleteTrail = DeleteTrail'
    { _dtName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTrail' smart constructor.
deleteTrail :: Text -> DeleteTrail
deleteTrail pName_ =
    DeleteTrail'
    { _dtName = pName_
    }

-- | The name of a trail to be deleted.
dtName :: Lens' DeleteTrail Text
dtName = lens _dtName (\ s a -> s{_dtName = a});

instance AWSRequest DeleteTrail where
        type Sv DeleteTrail = CloudTrail
        type Rs DeleteTrail = DeleteTrailResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTrailResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteTrail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.DeleteTrail"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTrail where
        toJSON DeleteTrail'{..} = object ["Name" .= _dtName]

instance ToPath DeleteTrail where
        toPath = const mempty

instance ToQuery DeleteTrail where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'deleteTrailResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsStatus'
newtype DeleteTrailResponse = DeleteTrailResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTrailResponse' smart constructor.
deleteTrailResponse :: Int -> DeleteTrailResponse
deleteTrailResponse pStatus_ =
    DeleteTrailResponse'
    { _drsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeleteTrailResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
