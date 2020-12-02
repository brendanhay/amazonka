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
-- Module      : Network.AWS.GuardDuty.DeleteIPSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the IPSet specified by the IPSet ID.
module Network.AWS.GuardDuty.DeleteIPSet
    (
    -- * Creating a Request
      deleteIPSet
    , DeleteIPSet
    -- * Request Lenses
    , disDetectorId
    , disIPSetId

    -- * Destructuring the Response
    , deleteIPSetResponse
    , DeleteIPSetResponse
    -- * Response Lenses
    , dipsrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteIPSet' smart constructor.
data DeleteIPSet = DeleteIPSet'
  { _disDetectorId :: !Text
  , _disIPSetId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disDetectorId' - The detectorID that specifies the GuardDuty service whose IPSet you want to delete.
--
-- * 'disIPSetId' - The unique ID that specifies the IPSet that you want to delete.
deleteIPSet
    :: Text -- ^ 'disDetectorId'
    -> Text -- ^ 'disIPSetId'
    -> DeleteIPSet
deleteIPSet pDetectorId_ pIPSetId_ =
  DeleteIPSet' {_disDetectorId = pDetectorId_, _disIPSetId = pIPSetId_}


-- | The detectorID that specifies the GuardDuty service whose IPSet you want to delete.
disDetectorId :: Lens' DeleteIPSet Text
disDetectorId = lens _disDetectorId (\ s a -> s{_disDetectorId = a})

-- | The unique ID that specifies the IPSet that you want to delete.
disIPSetId :: Lens' DeleteIPSet Text
disIPSetId = lens _disIPSetId (\ s a -> s{_disIPSetId = a})

instance AWSRequest DeleteIPSet where
        type Rs DeleteIPSet = DeleteIPSetResponse
        request = delete guardDuty
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteIPSetResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteIPSet where

instance NFData DeleteIPSet where

instance ToHeaders DeleteIPSet where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteIPSet where
        toPath DeleteIPSet'{..}
          = mconcat
              ["/detector/", toBS _disDetectorId, "/ipset/",
               toBS _disIPSetId]

instance ToQuery DeleteIPSet where
        toQuery = const mempty

-- | /See:/ 'deleteIPSetResponse' smart constructor.
newtype DeleteIPSetResponse = DeleteIPSetResponse'
  { _dipsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipsrsResponseStatus' - -- | The response status code.
deleteIPSetResponse
    :: Int -- ^ 'dipsrsResponseStatus'
    -> DeleteIPSetResponse
deleteIPSetResponse pResponseStatus_ =
  DeleteIPSetResponse' {_dipsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dipsrsResponseStatus :: Lens' DeleteIPSetResponse Int
dipsrsResponseStatus = lens _dipsrsResponseStatus (\ s a -> s{_dipsrsResponseStatus = a})

instance NFData DeleteIPSetResponse where
