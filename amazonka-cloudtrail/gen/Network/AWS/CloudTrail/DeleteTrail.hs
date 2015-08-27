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
-- Module      : Network.AWS.CloudTrail.DeleteTrail
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a trail.
--
-- /See:/ <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_DeleteTrail.html AWS API Reference> for DeleteTrail.
module Network.AWS.CloudTrail.DeleteTrail
    (
    -- * Creating a Request
      deleteTrail
    , DeleteTrail
    -- * Request Lenses
    , dtName

    -- * Destructuring the Response
    , deleteTrailResponse
    , DeleteTrailResponse
    -- * Response Lenses
    , drsStatus
    ) where

import           Network.AWS.CloudTrail.Types
import           Network.AWS.CloudTrail.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The request that specifies the name of a trail to delete.
--
-- /See:/ 'deleteTrail' smart constructor.
newtype DeleteTrail = DeleteTrail'
    { _dtName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTrail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtName'
deleteTrail
    :: Text -- ^ 'dtName'
    -> DeleteTrail
deleteTrail pName_ =
    DeleteTrail'
    { _dtName = pName_
    }

-- | The name of a trail to be deleted.
dtName :: Lens' DeleteTrail Text
dtName = lens _dtName (\ s a -> s{_dtName = a});

instance AWSRequest DeleteTrail where
        type Rs DeleteTrail = DeleteTrailResponse
        request = postJSON cloudTrail
        response
          = receiveEmpty
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
        toJSON DeleteTrail'{..}
          = object (catMaybes [Just ("Name" .= _dtName)])

instance ToPath DeleteTrail where
        toPath = const "/"

instance ToQuery DeleteTrail where
        toQuery = const mempty

-- | Returns the objects or data listed below if successful. Otherwise,
-- returns an error.
--
-- /See:/ 'deleteTrailResponse' smart constructor.
newtype DeleteTrailResponse = DeleteTrailResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteTrailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsStatus'
deleteTrailResponse
    :: Int -- ^ 'drsStatus'
    -> DeleteTrailResponse
deleteTrailResponse pStatus_ =
    DeleteTrailResponse'
    { _drsStatus = pStatus_
    }

-- | The response status code.
drsStatus :: Lens' DeleteTrailResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
