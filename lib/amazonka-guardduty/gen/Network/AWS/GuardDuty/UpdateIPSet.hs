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
-- Module      : Network.AWS.GuardDuty.UpdateIPSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the IPSet specified by the IPSet ID.
module Network.AWS.GuardDuty.UpdateIPSet
    (
    -- * Creating a Request
      updateIPSet
    , UpdateIPSet
    -- * Request Lenses
    , uisLocation
    , uisActivate
    , uisName
    , uisDetectorId
    , uisIPSetId

    -- * Destructuring the Response
    , updateIPSetResponse
    , UpdateIPSetResponse
    -- * Response Lenses
    , uisrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | UpdateIPSet request body.
--
-- /See:/ 'updateIPSet' smart constructor.
data UpdateIPSet = UpdateIPSet'
  { _uisLocation   :: !(Maybe Text)
  , _uisActivate   :: !(Maybe Bool)
  , _uisName       :: !(Maybe Text)
  , _uisDetectorId :: !Text
  , _uisIPSetId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uisLocation' - The updated URI of the file that contains the IPSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key).
--
-- * 'uisActivate' - The updated boolean value that specifies whether the IPSet is active or not.
--
-- * 'uisName' - The unique ID that specifies the IPSet that you want to update.
--
-- * 'uisDetectorId' - The detectorID that specifies the GuardDuty service whose IPSet you want to update.
--
-- * 'uisIPSetId' - The unique ID that specifies the IPSet that you want to update.
updateIPSet
    :: Text -- ^ 'uisDetectorId'
    -> Text -- ^ 'uisIPSetId'
    -> UpdateIPSet
updateIPSet pDetectorId_ pIPSetId_ =
  UpdateIPSet'
    { _uisLocation = Nothing
    , _uisActivate = Nothing
    , _uisName = Nothing
    , _uisDetectorId = pDetectorId_
    , _uisIPSetId = pIPSetId_
    }


-- | The updated URI of the file that contains the IPSet. For example (https://s3.us-west-2.amazonaws.com/my-bucket/my-object-key).
uisLocation :: Lens' UpdateIPSet (Maybe Text)
uisLocation = lens _uisLocation (\ s a -> s{_uisLocation = a})

-- | The updated boolean value that specifies whether the IPSet is active or not.
uisActivate :: Lens' UpdateIPSet (Maybe Bool)
uisActivate = lens _uisActivate (\ s a -> s{_uisActivate = a})

-- | The unique ID that specifies the IPSet that you want to update.
uisName :: Lens' UpdateIPSet (Maybe Text)
uisName = lens _uisName (\ s a -> s{_uisName = a})

-- | The detectorID that specifies the GuardDuty service whose IPSet you want to update.
uisDetectorId :: Lens' UpdateIPSet Text
uisDetectorId = lens _uisDetectorId (\ s a -> s{_uisDetectorId = a})

-- | The unique ID that specifies the IPSet that you want to update.
uisIPSetId :: Lens' UpdateIPSet Text
uisIPSetId = lens _uisIPSetId (\ s a -> s{_uisIPSetId = a})

instance AWSRequest UpdateIPSet where
        type Rs UpdateIPSet = UpdateIPSetResponse
        request = postJSON guardDuty
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateIPSetResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateIPSet where

instance NFData UpdateIPSet where

instance ToHeaders UpdateIPSet where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateIPSet where
        toJSON UpdateIPSet'{..}
          = object
              (catMaybes
                 [("location" .=) <$> _uisLocation,
                  ("activate" .=) <$> _uisActivate,
                  ("name" .=) <$> _uisName])

instance ToPath UpdateIPSet where
        toPath UpdateIPSet'{..}
          = mconcat
              ["/detector/", toBS _uisDetectorId, "/ipset/",
               toBS _uisIPSetId]

instance ToQuery UpdateIPSet where
        toQuery = const mempty

-- | /See:/ 'updateIPSetResponse' smart constructor.
newtype UpdateIPSetResponse = UpdateIPSetResponse'
  { _uisrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uisrsResponseStatus' - -- | The response status code.
updateIPSetResponse
    :: Int -- ^ 'uisrsResponseStatus'
    -> UpdateIPSetResponse
updateIPSetResponse pResponseStatus_ =
  UpdateIPSetResponse' {_uisrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uisrsResponseStatus :: Lens' UpdateIPSetResponse Int
uisrsResponseStatus = lens _uisrsResponseStatus (\ s a -> s{_uisrsResponseStatus = a})

instance NFData UpdateIPSetResponse where
