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
-- Module      : Network.AWS.MechanicalTurk.UpdateHITReviewStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITReviewStatus@ operation updates the status of a HIT. If the status is Reviewable, this operation can update the status to Reviewing, or it can revert a Reviewing HIT back to the Reviewable status.
--
--
module Network.AWS.MechanicalTurk.UpdateHITReviewStatus
    (
    -- * Creating a Request
      updateHITReviewStatus
    , UpdateHITReviewStatus
    -- * Request Lenses
    , uhitrsRevert
    , uhitrsHITId

    -- * Destructuring the Response
    , updateHITReviewStatusResponse
    , UpdateHITReviewStatusResponse
    -- * Response Lenses
    , uhitrsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateHITReviewStatus' smart constructor.
data UpdateHITReviewStatus = UpdateHITReviewStatus'
  { _uhitrsRevert :: !(Maybe Bool)
  , _uhitrsHITId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateHITReviewStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhitrsRevert' - Specifies how to update the HIT status. Default is @False@ .      * Setting this to false will only transition a HIT from @Reviewable@ to @Reviewing@      * Setting this to true will only transition a HIT from @Reviewing@ to @Reviewable@
--
-- * 'uhitrsHITId' - The ID of the HIT to update.
updateHITReviewStatus
    :: Text -- ^ 'uhitrsHITId'
    -> UpdateHITReviewStatus
updateHITReviewStatus pHITId_ =
  UpdateHITReviewStatus' {_uhitrsRevert = Nothing, _uhitrsHITId = pHITId_}


-- | Specifies how to update the HIT status. Default is @False@ .      * Setting this to false will only transition a HIT from @Reviewable@ to @Reviewing@      * Setting this to true will only transition a HIT from @Reviewing@ to @Reviewable@
uhitrsRevert :: Lens' UpdateHITReviewStatus (Maybe Bool)
uhitrsRevert = lens _uhitrsRevert (\ s a -> s{_uhitrsRevert = a})

-- | The ID of the HIT to update.
uhitrsHITId :: Lens' UpdateHITReviewStatus Text
uhitrsHITId = lens _uhitrsHITId (\ s a -> s{_uhitrsHITId = a})

instance AWSRequest UpdateHITReviewStatus where
        type Rs UpdateHITReviewStatus =
             UpdateHITReviewStatusResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateHITReviewStatusResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateHITReviewStatus where

instance NFData UpdateHITReviewStatus where

instance ToHeaders UpdateHITReviewStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.UpdateHITReviewStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateHITReviewStatus where
        toJSON UpdateHITReviewStatus'{..}
          = object
              (catMaybes
                 [("Revert" .=) <$> _uhitrsRevert,
                  Just ("HITId" .= _uhitrsHITId)])

instance ToPath UpdateHITReviewStatus where
        toPath = const "/"

instance ToQuery UpdateHITReviewStatus where
        toQuery = const mempty

-- | /See:/ 'updateHITReviewStatusResponse' smart constructor.
newtype UpdateHITReviewStatusResponse = UpdateHITReviewStatusResponse'
  { _uhitrsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateHITReviewStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uhitrsrsResponseStatus' - -- | The response status code.
updateHITReviewStatusResponse
    :: Int -- ^ 'uhitrsrsResponseStatus'
    -> UpdateHITReviewStatusResponse
updateHITReviewStatusResponse pResponseStatus_ =
  UpdateHITReviewStatusResponse' {_uhitrsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uhitrsrsResponseStatus :: Lens' UpdateHITReviewStatusResponse Int
uhitrsrsResponseStatus = lens _uhitrsrsResponseStatus (\ s a -> s{_uhitrsrsResponseStatus = a})

instance NFData UpdateHITReviewStatusResponse where
