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
-- Module      : Network.AWS.MechanicalTurk.SendBonus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @SendBonus@ operation issues a payment of money from your account to a Worker. This payment happens separately from the reward you pay to the Worker when you approve the Worker's assignment. The SendBonus operation requires the Worker's ID and the assignment ID as parameters to initiate payment of the bonus. You must include a message that explains the reason for the bonus payment, as the Worker may not be expecting the payment. Amazon Mechanical Turk collects a fee for bonus payments, similar to the HIT listing fee. This operation fails if your account does not have enough funds to pay for both the bonus and the fees.
--
--
module Network.AWS.MechanicalTurk.SendBonus
    (
    -- * Creating a Request
      sendBonus
    , SendBonus
    -- * Request Lenses
    , sbUniqueRequestToken
    , sbWorkerId
    , sbBonusAmount
    , sbAssignmentId
    , sbReason

    -- * Destructuring the Response
    , sendBonusResponse
    , SendBonusResponse
    -- * Response Lenses
    , sbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sendBonus' smart constructor.
data SendBonus = SendBonus'
  { _sbUniqueRequestToken :: !(Maybe Text)
  , _sbWorkerId           :: !Text
  , _sbBonusAmount        :: !Text
  , _sbAssignmentId       :: !Text
  , _sbReason             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendBonus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbUniqueRequestToken' - A unique identifier for this request, which allows you to retry the call on error without granting multiple bonuses. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the bonus already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return an error with a message containing the request ID.
--
-- * 'sbWorkerId' - The ID of the Worker being paid the bonus.
--
-- * 'sbBonusAmount' - The Bonus amount is a US Dollar amount specified using a string (for example, "5" represents $5.00 USD and "101.42" represents $101.42 USD). Do not include currency symbols or currency codes.
--
-- * 'sbAssignmentId' - The ID of the assignment for which this bonus is paid.
--
-- * 'sbReason' - A message that explains the reason for the bonus payment. The Worker receiving the bonus can see this message.
sendBonus
    :: Text -- ^ 'sbWorkerId'
    -> Text -- ^ 'sbBonusAmount'
    -> Text -- ^ 'sbAssignmentId'
    -> Text -- ^ 'sbReason'
    -> SendBonus
sendBonus pWorkerId_ pBonusAmount_ pAssignmentId_ pReason_ =
  SendBonus'
    { _sbUniqueRequestToken = Nothing
    , _sbWorkerId = pWorkerId_
    , _sbBonusAmount = pBonusAmount_
    , _sbAssignmentId = pAssignmentId_
    , _sbReason = pReason_
    }


-- | A unique identifier for this request, which allows you to retry the call on error without granting multiple bonuses. This is useful in cases such as network timeouts where it is unclear whether or not the call succeeded on the server. If the bonus already exists in the system from a previous call using the same UniqueRequestToken, subsequent calls will return an error with a message containing the request ID.
sbUniqueRequestToken :: Lens' SendBonus (Maybe Text)
sbUniqueRequestToken = lens _sbUniqueRequestToken (\ s a -> s{_sbUniqueRequestToken = a})

-- | The ID of the Worker being paid the bonus.
sbWorkerId :: Lens' SendBonus Text
sbWorkerId = lens _sbWorkerId (\ s a -> s{_sbWorkerId = a})

-- | The Bonus amount is a US Dollar amount specified using a string (for example, "5" represents $5.00 USD and "101.42" represents $101.42 USD). Do not include currency symbols or currency codes.
sbBonusAmount :: Lens' SendBonus Text
sbBonusAmount = lens _sbBonusAmount (\ s a -> s{_sbBonusAmount = a})

-- | The ID of the assignment for which this bonus is paid.
sbAssignmentId :: Lens' SendBonus Text
sbAssignmentId = lens _sbAssignmentId (\ s a -> s{_sbAssignmentId = a})

-- | A message that explains the reason for the bonus payment. The Worker receiving the bonus can see this message.
sbReason :: Lens' SendBonus Text
sbReason = lens _sbReason (\ s a -> s{_sbReason = a})

instance AWSRequest SendBonus where
        type Rs SendBonus = SendBonusResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 SendBonusResponse' <$> (pure (fromEnum s)))

instance Hashable SendBonus where

instance NFData SendBonus where

instance ToHeaders SendBonus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.SendBonus" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SendBonus where
        toJSON SendBonus'{..}
          = object
              (catMaybes
                 [("UniqueRequestToken" .=) <$> _sbUniqueRequestToken,
                  Just ("WorkerId" .= _sbWorkerId),
                  Just ("BonusAmount" .= _sbBonusAmount),
                  Just ("AssignmentId" .= _sbAssignmentId),
                  Just ("Reason" .= _sbReason)])

instance ToPath SendBonus where
        toPath = const "/"

instance ToQuery SendBonus where
        toQuery = const mempty

-- | /See:/ 'sendBonusResponse' smart constructor.
newtype SendBonusResponse = SendBonusResponse'
  { _sbrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendBonusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbrsResponseStatus' - -- | The response status code.
sendBonusResponse
    :: Int -- ^ 'sbrsResponseStatus'
    -> SendBonusResponse
sendBonusResponse pResponseStatus_ =
  SendBonusResponse' {_sbrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sbrsResponseStatus :: Lens' SendBonusResponse Int
sbrsResponseStatus = lens _sbrsResponseStatus (\ s a -> s{_sbrsResponseStatus = a})

instance NFData SendBonusResponse where
