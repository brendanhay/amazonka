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
-- Module      : Network.AWS.MechanicalTurk.DeleteQualificationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteQualificationType@ deletes a Qualification type and deletes any HIT types that are associated with the Qualification type.
--
--
-- This operation does not revoke Qualifications already assigned to Workers because the Qualifications might be needed for active HITs. If there are any pending requests for the Qualification type, Amazon Mechanical Turk rejects those requests. After you delete a Qualification type, you can no longer use it to create HITs or HIT types.
--
module Network.AWS.MechanicalTurk.DeleteQualificationType
    (
    -- * Creating a Request
      deleteQualificationType
    , DeleteQualificationType
    -- * Request Lenses
    , dqtQualificationTypeId

    -- * Destructuring the Response
    , deleteQualificationTypeResponse
    , DeleteQualificationTypeResponse
    -- * Response Lenses
    , dqtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteQualificationType' smart constructor.
newtype DeleteQualificationType = DeleteQualificationType'
  { _dqtQualificationTypeId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteQualificationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqtQualificationTypeId' - The ID of the QualificationType to dispose.
deleteQualificationType
    :: Text -- ^ 'dqtQualificationTypeId'
    -> DeleteQualificationType
deleteQualificationType pQualificationTypeId_ =
  DeleteQualificationType' {_dqtQualificationTypeId = pQualificationTypeId_}


-- | The ID of the QualificationType to dispose.
dqtQualificationTypeId :: Lens' DeleteQualificationType Text
dqtQualificationTypeId = lens _dqtQualificationTypeId (\ s a -> s{_dqtQualificationTypeId = a})

instance AWSRequest DeleteQualificationType where
        type Rs DeleteQualificationType =
             DeleteQualificationTypeResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteQualificationTypeResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteQualificationType where

instance NFData DeleteQualificationType where

instance ToHeaders DeleteQualificationType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.DeleteQualificationType"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteQualificationType where
        toJSON DeleteQualificationType'{..}
          = object
              (catMaybes
                 [Just
                    ("QualificationTypeId" .= _dqtQualificationTypeId)])

instance ToPath DeleteQualificationType where
        toPath = const "/"

instance ToQuery DeleteQualificationType where
        toQuery = const mempty

-- | /See:/ 'deleteQualificationTypeResponse' smart constructor.
newtype DeleteQualificationTypeResponse = DeleteQualificationTypeResponse'
  { _dqtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteQualificationTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqtrsResponseStatus' - -- | The response status code.
deleteQualificationTypeResponse
    :: Int -- ^ 'dqtrsResponseStatus'
    -> DeleteQualificationTypeResponse
deleteQualificationTypeResponse pResponseStatus_ =
  DeleteQualificationTypeResponse' {_dqtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dqtrsResponseStatus :: Lens' DeleteQualificationTypeResponse Int
dqtrsResponseStatus = lens _dqtrsResponseStatus (\ s a -> s{_dqtrsResponseStatus = a})

instance NFData DeleteQualificationTypeResponse where
