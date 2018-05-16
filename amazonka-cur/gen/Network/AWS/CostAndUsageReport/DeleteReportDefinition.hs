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
-- Module      : Network.AWS.CostAndUsageReport.DeleteReportDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a specified report definition
module Network.AWS.CostAndUsageReport.DeleteReportDefinition
    (
    -- * Creating a Request
      deleteReportDefinition
    , DeleteReportDefinition
    -- * Request Lenses
    , drdReportName

    -- * Destructuring the Response
    , deleteReportDefinitionResponse
    , DeleteReportDefinitionResponse
    -- * Response Lenses
    , drsResponseMessage
    , drsResponseStatus
    ) where

import Network.AWS.CostAndUsageReport.Types
import Network.AWS.CostAndUsageReport.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of DeleteReportDefinition
--
-- /See:/ 'deleteReportDefinition' smart constructor.
newtype DeleteReportDefinition = DeleteReportDefinition'
  { _drdReportName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReportDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdReportName' - Undocumented member.
deleteReportDefinition
    :: DeleteReportDefinition
deleteReportDefinition = DeleteReportDefinition' {_drdReportName = Nothing}


-- | Undocumented member.
drdReportName :: Lens' DeleteReportDefinition (Maybe Text)
drdReportName = lens _drdReportName (\ s a -> s{_drdReportName = a})

instance AWSRequest DeleteReportDefinition where
        type Rs DeleteReportDefinition =
             DeleteReportDefinitionResponse
        request = postJSON costAndUsageReport
        response
          = receiveJSON
              (\ s h x ->
                 DeleteReportDefinitionResponse' <$>
                   (x .?> "ResponseMessage") <*> (pure (fromEnum s)))

instance Hashable DeleteReportDefinition where

instance NFData DeleteReportDefinition where

instance ToHeaders DeleteReportDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrigamiServiceGatewayService.DeleteReportDefinition"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteReportDefinition where
        toJSON DeleteReportDefinition'{..}
          = object
              (catMaybes [("ReportName" .=) <$> _drdReportName])

instance ToPath DeleteReportDefinition where
        toPath = const "/"

instance ToQuery DeleteReportDefinition where
        toQuery = const mempty

-- | Response of DeleteReportDefinition
--
-- /See:/ 'deleteReportDefinitionResponse' smart constructor.
data DeleteReportDefinitionResponse = DeleteReportDefinitionResponse'
  { _drsResponseMessage :: !(Maybe Text)
  , _drsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteReportDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseMessage' - Undocumented member.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteReportDefinitionResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteReportDefinitionResponse
deleteReportDefinitionResponse pResponseStatus_ =
  DeleteReportDefinitionResponse'
    {_drsResponseMessage = Nothing, _drsResponseStatus = pResponseStatus_}


-- | Undocumented member.
drsResponseMessage :: Lens' DeleteReportDefinitionResponse (Maybe Text)
drsResponseMessage = lens _drsResponseMessage (\ s a -> s{_drsResponseMessage = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteReportDefinitionResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteReportDefinitionResponse where
