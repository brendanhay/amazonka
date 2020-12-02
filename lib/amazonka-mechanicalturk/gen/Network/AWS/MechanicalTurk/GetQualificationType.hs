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
-- Module      : Network.AWS.MechanicalTurk.GetQualificationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationType@ operation retrieves information about a Qualification type using its ID.
--
--
module Network.AWS.MechanicalTurk.GetQualificationType
    (
    -- * Creating a Request
      getQualificationType
    , GetQualificationType
    -- * Request Lenses
    , gqtQualificationTypeId

    -- * Destructuring the Response
    , getQualificationTypeResponse
    , GetQualificationTypeResponse
    -- * Response Lenses
    , gqtrsQualificationType
    , gqtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getQualificationType' smart constructor.
newtype GetQualificationType = GetQualificationType'
  { _gqtQualificationTypeId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQualificationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqtQualificationTypeId' - The ID of the QualificationType.
getQualificationType
    :: Text -- ^ 'gqtQualificationTypeId'
    -> GetQualificationType
getQualificationType pQualificationTypeId_ =
  GetQualificationType' {_gqtQualificationTypeId = pQualificationTypeId_}


-- | The ID of the QualificationType.
gqtQualificationTypeId :: Lens' GetQualificationType Text
gqtQualificationTypeId = lens _gqtQualificationTypeId (\ s a -> s{_gqtQualificationTypeId = a})

instance AWSRequest GetQualificationType where
        type Rs GetQualificationType =
             GetQualificationTypeResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 GetQualificationTypeResponse' <$>
                   (x .?> "QualificationType") <*> (pure (fromEnum s)))

instance Hashable GetQualificationType where

instance NFData GetQualificationType where

instance ToHeaders GetQualificationType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.GetQualificationType"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetQualificationType where
        toJSON GetQualificationType'{..}
          = object
              (catMaybes
                 [Just
                    ("QualificationTypeId" .= _gqtQualificationTypeId)])

instance ToPath GetQualificationType where
        toPath = const "/"

instance ToQuery GetQualificationType where
        toQuery = const mempty

-- | /See:/ 'getQualificationTypeResponse' smart constructor.
data GetQualificationTypeResponse = GetQualificationTypeResponse'
  { _gqtrsQualificationType :: !(Maybe QualificationType)
  , _gqtrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQualificationTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqtrsQualificationType' - The returned Qualification Type
--
-- * 'gqtrsResponseStatus' - -- | The response status code.
getQualificationTypeResponse
    :: Int -- ^ 'gqtrsResponseStatus'
    -> GetQualificationTypeResponse
getQualificationTypeResponse pResponseStatus_ =
  GetQualificationTypeResponse'
    {_gqtrsQualificationType = Nothing, _gqtrsResponseStatus = pResponseStatus_}


-- | The returned Qualification Type
gqtrsQualificationType :: Lens' GetQualificationTypeResponse (Maybe QualificationType)
gqtrsQualificationType = lens _gqtrsQualificationType (\ s a -> s{_gqtrsQualificationType = a})

-- | -- | The response status code.
gqtrsResponseStatus :: Lens' GetQualificationTypeResponse Int
gqtrsResponseStatus = lens _gqtrsResponseStatus (\ s a -> s{_gqtrsResponseStatus = a})

instance NFData GetQualificationTypeResponse where
