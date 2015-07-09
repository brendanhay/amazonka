{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteFlowLogs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more flow logs.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteFlowLogs.html>
module Network.AWS.EC2.DeleteFlowLogs
    (
    -- * Request
      DeleteFlowLogs
    -- ** Request constructor
    , deleteFlowLogs
    -- ** Request lenses
    , dflFlowLogIds

    -- * Response
    , DeleteFlowLogsResponse
    -- ** Response constructor
    , deleteFlowLogsResponse
    -- ** Response lenses
    , dflrUnsuccessful
    , dflrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteFlowLogs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dflFlowLogIds'
newtype DeleteFlowLogs = DeleteFlowLogs'
    { _dflFlowLogIds :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteFlowLogs' smart constructor.
deleteFlowLogs :: DeleteFlowLogs
deleteFlowLogs =
    DeleteFlowLogs'
    { _dflFlowLogIds = mempty
    }

-- | One or more flow log IDs.
dflFlowLogIds :: Lens' DeleteFlowLogs [Text]
dflFlowLogIds = lens _dflFlowLogIds (\ s a -> s{_dflFlowLogIds = a});

instance AWSRequest DeleteFlowLogs where
        type Sv DeleteFlowLogs = EC2
        type Rs DeleteFlowLogs = DeleteFlowLogsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DeleteFlowLogsResponse' <$>
                   (x .@? "unsuccessful" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DeleteFlowLogs where
        toHeaders = const mempty

instance ToPath DeleteFlowLogs where
        toPath = const "/"

instance ToQuery DeleteFlowLogs where
        toQuery DeleteFlowLogs'{..}
          = mconcat
              ["Action" =: ("DeleteFlowLogs" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQueryList "item" _dflFlowLogIds]

-- | /See:/ 'deleteFlowLogsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dflrUnsuccessful'
--
-- * 'dflrStatus'
data DeleteFlowLogsResponse = DeleteFlowLogsResponse'
    { _dflrUnsuccessful :: !(Maybe [UnsuccessfulItem])
    , _dflrStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteFlowLogsResponse' smart constructor.
deleteFlowLogsResponse :: Int -> DeleteFlowLogsResponse
deleteFlowLogsResponse pStatus =
    DeleteFlowLogsResponse'
    { _dflrUnsuccessful = Nothing
    , _dflrStatus = pStatus
    }

-- | Information about the flow logs that could not be deleted successfully.
dflrUnsuccessful :: Lens' DeleteFlowLogsResponse [UnsuccessfulItem]
dflrUnsuccessful = lens _dflrUnsuccessful (\ s a -> s{_dflrUnsuccessful = a}) . _Default;

-- | FIXME: Undocumented member.
dflrStatus :: Lens' DeleteFlowLogsResponse Int
dflrStatus = lens _dflrStatus (\ s a -> s{_dflrStatus = a});
