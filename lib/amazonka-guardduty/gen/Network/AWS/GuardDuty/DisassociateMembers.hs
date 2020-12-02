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
-- Module      : Network.AWS.GuardDuty.DisassociateMembers
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates GuardDuty member accounts (to the current GuardDuty master account) specified by the account IDs.
module Network.AWS.GuardDuty.DisassociateMembers
    (
    -- * Creating a Request
      disassociateMembers
    , DisassociateMembers
    -- * Request Lenses
    , dmsAccountIds
    , dmsDetectorId

    -- * Destructuring the Response
    , disassociateMembersResponse
    , DisassociateMembersResponse
    -- * Response Lenses
    , dmrsUnprocessedAccounts
    , dmrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | DisassociateMembers request body.
--
-- /See:/ 'disassociateMembers' smart constructor.
data DisassociateMembers = DisassociateMembers'
  { _dmsAccountIds :: !(Maybe [Text])
  , _dmsDetectorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateMembers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmsAccountIds' - A list of account IDs of the GuardDuty member accounts that you want to disassociate from master.
--
-- * 'dmsDetectorId' - The unique ID of the detector of the GuardDuty account whose members you want to disassociate from master.
disassociateMembers
    :: Text -- ^ 'dmsDetectorId'
    -> DisassociateMembers
disassociateMembers pDetectorId_ =
  DisassociateMembers' {_dmsAccountIds = Nothing, _dmsDetectorId = pDetectorId_}


-- | A list of account IDs of the GuardDuty member accounts that you want to disassociate from master.
dmsAccountIds :: Lens' DisassociateMembers [Text]
dmsAccountIds = lens _dmsAccountIds (\ s a -> s{_dmsAccountIds = a}) . _Default . _Coerce

-- | The unique ID of the detector of the GuardDuty account whose members you want to disassociate from master.
dmsDetectorId :: Lens' DisassociateMembers Text
dmsDetectorId = lens _dmsDetectorId (\ s a -> s{_dmsDetectorId = a})

instance AWSRequest DisassociateMembers where
        type Rs DisassociateMembers =
             DisassociateMembersResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 DisassociateMembersResponse' <$>
                   (x .?> "unprocessedAccounts" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DisassociateMembers where

instance NFData DisassociateMembers where

instance ToHeaders DisassociateMembers where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateMembers where
        toJSON DisassociateMembers'{..}
          = object
              (catMaybes [("accountIds" .=) <$> _dmsAccountIds])

instance ToPath DisassociateMembers where
        toPath DisassociateMembers'{..}
          = mconcat
              ["/detector/", toBS _dmsDetectorId,
               "/member/disassociate"]

instance ToQuery DisassociateMembers where
        toQuery = const mempty

-- | /See:/ 'disassociateMembersResponse' smart constructor.
data DisassociateMembersResponse = DisassociateMembersResponse'
  { _dmrsUnprocessedAccounts :: !(Maybe [UnprocessedAccount])
  , _dmrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateMembersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsUnprocessedAccounts' - A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
--
-- * 'dmrsResponseStatus' - -- | The response status code.
disassociateMembersResponse
    :: Int -- ^ 'dmrsResponseStatus'
    -> DisassociateMembersResponse
disassociateMembersResponse pResponseStatus_ =
  DisassociateMembersResponse'
    {_dmrsUnprocessedAccounts = Nothing, _dmrsResponseStatus = pResponseStatus_}


-- | A list of objects containing the unprocessed account and a result string explaining why it was unprocessed.
dmrsUnprocessedAccounts :: Lens' DisassociateMembersResponse [UnprocessedAccount]
dmrsUnprocessedAccounts = lens _dmrsUnprocessedAccounts (\ s a -> s{_dmrsUnprocessedAccounts = a}) . _Default . _Coerce

-- | -- | The response status code.
dmrsResponseStatus :: Lens' DisassociateMembersResponse Int
dmrsResponseStatus = lens _dmrsResponseStatus (\ s a -> s{_dmrsResponseStatus = a})

instance NFData DisassociateMembersResponse where
