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
-- Module      : Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @AssociateQualificationWithWorker@ operation gives a Worker a Qualification. @AssociateQualificationWithWorker@ does not require that the Worker submit a Qualification request. It gives the Qualification directly to the Worker.
--
--
-- You can only assign a Qualification of a Qualification type that you created (using the @CreateQualificationType@ operation).
--
module Network.AWS.MechanicalTurk.AssociateQualificationWithWorker
    (
    -- * Creating a Request
      associateQualificationWithWorker
    , AssociateQualificationWithWorker
    -- * Request Lenses
    , aqwwIntegerValue
    , aqwwSendNotification
    , aqwwQualificationTypeId
    , aqwwWorkerId

    -- * Destructuring the Response
    , associateQualificationWithWorkerResponse
    , AssociateQualificationWithWorkerResponse
    -- * Response Lenses
    , aqwwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateQualificationWithWorker' smart constructor.
data AssociateQualificationWithWorker = AssociateQualificationWithWorker'
  { _aqwwIntegerValue        :: !(Maybe Int)
  , _aqwwSendNotification    :: !(Maybe Bool)
  , _aqwwQualificationTypeId :: !Text
  , _aqwwWorkerId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateQualificationWithWorker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqwwIntegerValue' - The value of the Qualification to assign.
--
-- * 'aqwwSendNotification' - Specifies whether to send a notification email message to the Worker saying that the qualification was assigned to the Worker. Note: this is true by default.
--
-- * 'aqwwQualificationTypeId' - The ID of the Qualification type to use for the assigned Qualification.
--
-- * 'aqwwWorkerId' - The ID of the Worker to whom the Qualification is being assigned. Worker IDs are included with submitted HIT assignments and Qualification requests.
associateQualificationWithWorker
    :: Text -- ^ 'aqwwQualificationTypeId'
    -> Text -- ^ 'aqwwWorkerId'
    -> AssociateQualificationWithWorker
associateQualificationWithWorker pQualificationTypeId_ pWorkerId_ =
  AssociateQualificationWithWorker'
    { _aqwwIntegerValue = Nothing
    , _aqwwSendNotification = Nothing
    , _aqwwQualificationTypeId = pQualificationTypeId_
    , _aqwwWorkerId = pWorkerId_
    }


-- | The value of the Qualification to assign.
aqwwIntegerValue :: Lens' AssociateQualificationWithWorker (Maybe Int)
aqwwIntegerValue = lens _aqwwIntegerValue (\ s a -> s{_aqwwIntegerValue = a})

-- | Specifies whether to send a notification email message to the Worker saying that the qualification was assigned to the Worker. Note: this is true by default.
aqwwSendNotification :: Lens' AssociateQualificationWithWorker (Maybe Bool)
aqwwSendNotification = lens _aqwwSendNotification (\ s a -> s{_aqwwSendNotification = a})

-- | The ID of the Qualification type to use for the assigned Qualification.
aqwwQualificationTypeId :: Lens' AssociateQualificationWithWorker Text
aqwwQualificationTypeId = lens _aqwwQualificationTypeId (\ s a -> s{_aqwwQualificationTypeId = a})

-- | The ID of the Worker to whom the Qualification is being assigned. Worker IDs are included with submitted HIT assignments and Qualification requests.
aqwwWorkerId :: Lens' AssociateQualificationWithWorker Text
aqwwWorkerId = lens _aqwwWorkerId (\ s a -> s{_aqwwWorkerId = a})

instance AWSRequest AssociateQualificationWithWorker
         where
        type Rs AssociateQualificationWithWorker =
             AssociateQualificationWithWorkerResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 AssociateQualificationWithWorkerResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AssociateQualificationWithWorker
         where

instance NFData AssociateQualificationWithWorker
         where

instance ToHeaders AssociateQualificationWithWorker
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.AssociateQualificationWithWorker"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateQualificationWithWorker
         where
        toJSON AssociateQualificationWithWorker'{..}
          = object
              (catMaybes
                 [("IntegerValue" .=) <$> _aqwwIntegerValue,
                  ("SendNotification" .=) <$> _aqwwSendNotification,
                  Just
                    ("QualificationTypeId" .= _aqwwQualificationTypeId),
                  Just ("WorkerId" .= _aqwwWorkerId)])

instance ToPath AssociateQualificationWithWorker
         where
        toPath = const "/"

instance ToQuery AssociateQualificationWithWorker
         where
        toQuery = const mempty

-- | /See:/ 'associateQualificationWithWorkerResponse' smart constructor.
newtype AssociateQualificationWithWorkerResponse = AssociateQualificationWithWorkerResponse'
  { _aqwwrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateQualificationWithWorkerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aqwwrsResponseStatus' - -- | The response status code.
associateQualificationWithWorkerResponse
    :: Int -- ^ 'aqwwrsResponseStatus'
    -> AssociateQualificationWithWorkerResponse
associateQualificationWithWorkerResponse pResponseStatus_ =
  AssociateQualificationWithWorkerResponse'
    {_aqwwrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aqwwrsResponseStatus :: Lens' AssociateQualificationWithWorkerResponse Int
aqwwrsResponseStatus = lens _aqwwrsResponseStatus (\ s a -> s{_aqwwrsResponseStatus = a})

instance NFData
           AssociateQualificationWithWorkerResponse
         where
