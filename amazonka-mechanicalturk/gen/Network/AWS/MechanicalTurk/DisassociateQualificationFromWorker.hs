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
-- Module      : Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DisassociateQualificationFromWorker@ revokes a previously granted Qualification from a user.
--
--
-- You can provide a text message explaining why the Qualification was revoked. The user who had the Qualification can see this message.
--
module Network.AWS.MechanicalTurk.DisassociateQualificationFromWorker
    (
    -- * Creating a Request
      disassociateQualificationFromWorker
    , DisassociateQualificationFromWorker
    -- * Request Lenses
    , dqfwReason
    , dqfwWorkerId
    , dqfwQualificationTypeId

    -- * Destructuring the Response
    , disassociateQualificationFromWorkerResponse
    , DisassociateQualificationFromWorkerResponse
    -- * Response Lenses
    , dqfwrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateQualificationFromWorker' smart constructor.
data DisassociateQualificationFromWorker = DisassociateQualificationFromWorker'
  { _dqfwReason              :: !(Maybe Text)
  , _dqfwWorkerId            :: !Text
  , _dqfwQualificationTypeId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateQualificationFromWorker' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqfwReason' - A text message that explains why the Qualification was revoked. The user who had the Qualification sees this message.
--
-- * 'dqfwWorkerId' - The ID of the Worker who possesses the Qualification to be revoked.
--
-- * 'dqfwQualificationTypeId' - The ID of the Qualification type of the Qualification to be revoked.
disassociateQualificationFromWorker
    :: Text -- ^ 'dqfwWorkerId'
    -> Text -- ^ 'dqfwQualificationTypeId'
    -> DisassociateQualificationFromWorker
disassociateQualificationFromWorker pWorkerId_ pQualificationTypeId_ =
  DisassociateQualificationFromWorker'
    { _dqfwReason = Nothing
    , _dqfwWorkerId = pWorkerId_
    , _dqfwQualificationTypeId = pQualificationTypeId_
    }


-- | A text message that explains why the Qualification was revoked. The user who had the Qualification sees this message.
dqfwReason :: Lens' DisassociateQualificationFromWorker (Maybe Text)
dqfwReason = lens _dqfwReason (\ s a -> s{_dqfwReason = a})

-- | The ID of the Worker who possesses the Qualification to be revoked.
dqfwWorkerId :: Lens' DisassociateQualificationFromWorker Text
dqfwWorkerId = lens _dqfwWorkerId (\ s a -> s{_dqfwWorkerId = a})

-- | The ID of the Qualification type of the Qualification to be revoked.
dqfwQualificationTypeId :: Lens' DisassociateQualificationFromWorker Text
dqfwQualificationTypeId = lens _dqfwQualificationTypeId (\ s a -> s{_dqfwQualificationTypeId = a})

instance AWSRequest
           DisassociateQualificationFromWorker
         where
        type Rs DisassociateQualificationFromWorker =
             DisassociateQualificationFromWorkerResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateQualificationFromWorkerResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateQualificationFromWorker
         where

instance NFData DisassociateQualificationFromWorker
         where

instance ToHeaders
           DisassociateQualificationFromWorker
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.DisassociateQualificationFromWorker"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateQualificationFromWorker
         where
        toJSON DisassociateQualificationFromWorker'{..}
          = object
              (catMaybes
                 [("Reason" .=) <$> _dqfwReason,
                  Just ("WorkerId" .= _dqfwWorkerId),
                  Just
                    ("QualificationTypeId" .= _dqfwQualificationTypeId)])

instance ToPath DisassociateQualificationFromWorker
         where
        toPath = const "/"

instance ToQuery DisassociateQualificationFromWorker
         where
        toQuery = const mempty

-- | /See:/ 'disassociateQualificationFromWorkerResponse' smart constructor.
newtype DisassociateQualificationFromWorkerResponse = DisassociateQualificationFromWorkerResponse'
  { _dqfwrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateQualificationFromWorkerResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqfwrsResponseStatus' - -- | The response status code.
disassociateQualificationFromWorkerResponse
    :: Int -- ^ 'dqfwrsResponseStatus'
    -> DisassociateQualificationFromWorkerResponse
disassociateQualificationFromWorkerResponse pResponseStatus_ =
  DisassociateQualificationFromWorkerResponse'
    {_dqfwrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dqfwrsResponseStatus :: Lens' DisassociateQualificationFromWorkerResponse Int
dqfwrsResponseStatus = lens _dqfwrsResponseStatus (\ s a -> s{_dqfwrsResponseStatus = a})

instance NFData
           DisassociateQualificationFromWorkerResponse
         where
