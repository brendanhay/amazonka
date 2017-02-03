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
-- Module      : Network.AWS.Discovery.ExportConfigurations
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports all discovered configuration data to an Amazon S3 bucket or an application that enables you to view and evaluate the data. Data includes tags and tag associations, processes, connections, servers, and system performance. This API returns an export ID which you can query using the /DescribeExportConfigurations/ API. The system imposes a limit of two configuration exports in six hours.
--
--
module Network.AWS.Discovery.ExportConfigurations
    (
    -- * Creating a Request
      exportConfigurations
    , ExportConfigurations

    -- * Destructuring the Response
    , exportConfigurationsResponse
    , ExportConfigurationsResponse
    -- * Response Lenses
    , ecrsExportId
    , ecrsResponseStatus
    ) where

import           Network.AWS.Discovery.Types
import           Network.AWS.Discovery.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'exportConfigurations' smart constructor.
data ExportConfigurations =
    ExportConfigurations'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExportConfigurations' with the minimum fields required to make a request.
--
exportConfigurations
    :: ExportConfigurations
exportConfigurations = ExportConfigurations'

instance AWSRequest ExportConfigurations where
        type Rs ExportConfigurations =
             ExportConfigurationsResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 ExportConfigurationsResponse' <$>
                   (x .?> "exportId") <*> (pure (fromEnum s)))

instance Hashable ExportConfigurations

instance NFData ExportConfigurations

instance ToHeaders ExportConfigurations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.ExportConfigurations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ExportConfigurations where
        toJSON = const (Object mempty)

instance ToPath ExportConfigurations where
        toPath = const "/"

instance ToQuery ExportConfigurations where
        toQuery = const mempty

-- | /See:/ 'exportConfigurationsResponse' smart constructor.
data ExportConfigurationsResponse = ExportConfigurationsResponse'
    { _ecrsExportId       :: !(Maybe Text)
    , _ecrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExportConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecrsExportId' - A unique identifier that you can use to query the export status.
--
-- * 'ecrsResponseStatus' - -- | The response status code.
exportConfigurationsResponse
    :: Int -- ^ 'ecrsResponseStatus'
    -> ExportConfigurationsResponse
exportConfigurationsResponse pResponseStatus_ =
    ExportConfigurationsResponse'
    { _ecrsExportId = Nothing
    , _ecrsResponseStatus = pResponseStatus_
    }

-- | A unique identifier that you can use to query the export status.
ecrsExportId :: Lens' ExportConfigurationsResponse (Maybe Text)
ecrsExportId = lens _ecrsExportId (\ s a -> s{_ecrsExportId = a});

-- | -- | The response status code.
ecrsResponseStatus :: Lens' ExportConfigurationsResponse Int
ecrsResponseStatus = lens _ecrsResponseStatus (\ s a -> s{_ecrsResponseStatus = a});

instance NFData ExportConfigurationsResponse
