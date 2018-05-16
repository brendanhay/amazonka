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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a trail. This operation must be called from the region in which the trail was created. @DeleteTrail@ cannot be called on the shadow trails (replicated trails in other regions) of a trail that is enabled in all regions.
--
--
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
    , drsResponseStatus
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.CloudTrail.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request that specifies the name of a trail to delete.
--
--
--
-- /See:/ 'deleteTrail' smart constructor.
newtype DeleteTrail = DeleteTrail'
  { _dtName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTrail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtName' - Specifies the name or the CloudTrail ARN of the trail to be deleted. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
deleteTrail
    :: Text -- ^ 'dtName'
    -> DeleteTrail
deleteTrail pName_ = DeleteTrail' {_dtName = pName_}


-- | Specifies the name or the CloudTrail ARN of the trail to be deleted. The format of a trail ARN is: @arn:aws:cloudtrail:us-east-1:123456789012:trail/MyTrail@
dtName :: Lens' DeleteTrail Text
dtName = lens _dtName (\ s a -> s{_dtName = a})

instance AWSRequest DeleteTrail where
        type Rs DeleteTrail = DeleteTrailResponse
        request = postJSON cloudTrail
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteTrailResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteTrail where

instance NFData DeleteTrail where

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

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
--
--
-- /See:/ 'deleteTrailResponse' smart constructor.
newtype DeleteTrailResponse = DeleteTrailResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTrailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteTrailResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteTrailResponse
deleteTrailResponse pResponseStatus_ =
  DeleteTrailResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteTrailResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteTrailResponse where
