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
-- Module      : Network.AWS.CostAndUsageReport.PutReportDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new report definition
module Network.AWS.CostAndUsageReport.PutReportDefinition
    (
    -- * Creating a Request
      putReportDefinition
    , PutReportDefinition
    -- * Request Lenses
    , prdReportDefinition

    -- * Destructuring the Response
    , putReportDefinitionResponse
    , PutReportDefinitionResponse
    -- * Response Lenses
    , prdrsResponseStatus
    ) where

import Network.AWS.CostAndUsageReport.Types
import Network.AWS.CostAndUsageReport.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of PutReportDefinition
--
-- /See:/ 'putReportDefinition' smart constructor.
newtype PutReportDefinition = PutReportDefinition'
  { _prdReportDefinition :: ReportDefinition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutReportDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prdReportDefinition' - Undocumented member.
putReportDefinition
    :: ReportDefinition -- ^ 'prdReportDefinition'
    -> PutReportDefinition
putReportDefinition pReportDefinition_ =
  PutReportDefinition' {_prdReportDefinition = pReportDefinition_}


-- | Undocumented member.
prdReportDefinition :: Lens' PutReportDefinition ReportDefinition
prdReportDefinition = lens _prdReportDefinition (\ s a -> s{_prdReportDefinition = a})

instance AWSRequest PutReportDefinition where
        type Rs PutReportDefinition =
             PutReportDefinitionResponse
        request = postJSON costAndUsageReport
        response
          = receiveEmpty
              (\ s h x ->
                 PutReportDefinitionResponse' <$> (pure (fromEnum s)))

instance Hashable PutReportDefinition where

instance NFData PutReportDefinition where

instance ToHeaders PutReportDefinition where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrigamiServiceGatewayService.PutReportDefinition"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutReportDefinition where
        toJSON PutReportDefinition'{..}
          = object
              (catMaybes
                 [Just ("ReportDefinition" .= _prdReportDefinition)])

instance ToPath PutReportDefinition where
        toPath = const "/"

instance ToQuery PutReportDefinition where
        toQuery = const mempty

-- | Response of PutReportDefinition
--
-- /See:/ 'putReportDefinitionResponse' smart constructor.
newtype PutReportDefinitionResponse = PutReportDefinitionResponse'
  { _prdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutReportDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prdrsResponseStatus' - -- | The response status code.
putReportDefinitionResponse
    :: Int -- ^ 'prdrsResponseStatus'
    -> PutReportDefinitionResponse
putReportDefinitionResponse pResponseStatus_ =
  PutReportDefinitionResponse' {_prdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
prdrsResponseStatus :: Lens' PutReportDefinitionResponse Int
prdrsResponseStatus = lens _prdrsResponseStatus (\ s a -> s{_prdrsResponseStatus = a})

instance NFData PutReportDefinitionResponse where
