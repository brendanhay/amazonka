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
-- Module      : Network.AWS.EC2.DeleteFlowLogs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more flow logs.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteFlowLogs.html AWS API Reference> for DeleteFlowLogs.
module Network.AWS.EC2.DeleteFlowLogs
    (
    -- * Creating a Request
      deleteFlowLogs
    , DeleteFlowLogs
    -- * Request Lenses
    , dflFlowLogIds

    -- * Destructuring the Response
    , deleteFlowLogsResponse
    , DeleteFlowLogsResponse
    -- * Response Lenses
    , dflrsUnsuccessful
    , dflrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteFlowLogs' smart constructor.
newtype DeleteFlowLogs = DeleteFlowLogs'
    { _dflFlowLogIds :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteFlowLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dflFlowLogIds'
deleteFlowLogs
    :: DeleteFlowLogs
deleteFlowLogs =
    DeleteFlowLogs'
    { _dflFlowLogIds = mempty
    }

-- | One or more flow log IDs.
dflFlowLogIds :: Lens' DeleteFlowLogs [Text]
dflFlowLogIds = lens _dflFlowLogIds (\ s a -> s{_dflFlowLogIds = a}) . _Coerce;

instance AWSRequest DeleteFlowLogs where
        type Rs DeleteFlowLogs = DeleteFlowLogsResponse
        request = postQuery eC2
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
               toQueryList "FlowLogId" _dflFlowLogIds]

-- | /See:/ 'deleteFlowLogsResponse' smart constructor.
data DeleteFlowLogsResponse = DeleteFlowLogsResponse'
    { _dflrsUnsuccessful :: !(Maybe [UnsuccessfulItem])
    , _dflrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteFlowLogsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dflrsUnsuccessful'
--
-- * 'dflrsStatus'
deleteFlowLogsResponse
    :: Int -- ^ 'dflrsStatus'
    -> DeleteFlowLogsResponse
deleteFlowLogsResponse pStatus_ =
    DeleteFlowLogsResponse'
    { _dflrsUnsuccessful = Nothing
    , _dflrsStatus = pStatus_
    }

-- | Information about the flow logs that could not be deleted successfully.
dflrsUnsuccessful :: Lens' DeleteFlowLogsResponse [UnsuccessfulItem]
dflrsUnsuccessful = lens _dflrsUnsuccessful (\ s a -> s{_dflrsUnsuccessful = a}) . _Default . _Coerce;

-- | The response status code.
dflrsStatus :: Lens' DeleteFlowLogsResponse Int
dflrsStatus = lens _dflrsStatus (\ s a -> s{_dflrsStatus = a});
