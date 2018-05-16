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
-- Module      : Network.AWS.GuardDuty.DeleteFilter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the filter specified by the filter name.
module Network.AWS.GuardDuty.DeleteFilter
    (
    -- * Creating a Request
      deleteFilter
    , DeleteFilter
    -- * Request Lenses
    , dfDetectorId
    , dfFilterName

    -- * Destructuring the Response
    , deleteFilterResponse
    , DeleteFilterResponse
    -- * Response Lenses
    , dfrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFilter' smart constructor.
data DeleteFilter = DeleteFilter'
  { _dfDetectorId :: !Text
  , _dfFilterName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfDetectorId' - The unique ID that specifies the detector where you want to delete a filter.
--
-- * 'dfFilterName' - The name of the filter.
deleteFilter
    :: Text -- ^ 'dfDetectorId'
    -> Text -- ^ 'dfFilterName'
    -> DeleteFilter
deleteFilter pDetectorId_ pFilterName_ =
  DeleteFilter' {_dfDetectorId = pDetectorId_, _dfFilterName = pFilterName_}


-- | The unique ID that specifies the detector where you want to delete a filter.
dfDetectorId :: Lens' DeleteFilter Text
dfDetectorId = lens _dfDetectorId (\ s a -> s{_dfDetectorId = a})

-- | The name of the filter.
dfFilterName :: Lens' DeleteFilter Text
dfFilterName = lens _dfFilterName (\ s a -> s{_dfFilterName = a})

instance AWSRequest DeleteFilter where
        type Rs DeleteFilter = DeleteFilterResponse
        request = delete guardDuty
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteFilterResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteFilter where

instance NFData DeleteFilter where

instance ToHeaders DeleteFilter where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteFilter where
        toPath DeleteFilter'{..}
          = mconcat
              ["/detector/", toBS _dfDetectorId, "/filter/",
               toBS _dfFilterName]

instance ToQuery DeleteFilter where
        toQuery = const mempty

-- | /See:/ 'deleteFilterResponse' smart constructor.
newtype DeleteFilterResponse = DeleteFilterResponse'
  { _dfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFilterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfrsResponseStatus' - -- | The response status code.
deleteFilterResponse
    :: Int -- ^ 'dfrsResponseStatus'
    -> DeleteFilterResponse
deleteFilterResponse pResponseStatus_ =
  DeleteFilterResponse' {_dfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dfrsResponseStatus :: Lens' DeleteFilterResponse Int
dfrsResponseStatus = lens _dfrsResponseStatus (\ s a -> s{_dfrsResponseStatus = a})

instance NFData DeleteFilterResponse where
