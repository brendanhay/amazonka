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
-- Module      : Network.AWS.Route53.DeleteQueryLoggingConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configuration for DNS query logging. If you delete a configuration, Amazon Route 53 stops sending query logs to CloudWatch Logs. Amazon Route 53 doesn't delete any logs that are already in CloudWatch Logs.
--
--
-- For more information about DNS query logs, see 'CreateQueryLoggingConfig' .
--
module Network.AWS.Route53.DeleteQueryLoggingConfig
    (
    -- * Creating a Request
      deleteQueryLoggingConfig
    , DeleteQueryLoggingConfig
    -- * Request Lenses
    , dqlcId

    -- * Destructuring the Response
    , deleteQueryLoggingConfigResponse
    , DeleteQueryLoggingConfigResponse
    -- * Response Lenses
    , dqlcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | /See:/ 'deleteQueryLoggingConfig' smart constructor.
newtype DeleteQueryLoggingConfig = DeleteQueryLoggingConfig'
  { _dqlcId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteQueryLoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqlcId' - The ID of the configuration that you want to delete.
deleteQueryLoggingConfig
    :: Text -- ^ 'dqlcId'
    -> DeleteQueryLoggingConfig
deleteQueryLoggingConfig pId_ = DeleteQueryLoggingConfig' {_dqlcId = pId_}


-- | The ID of the configuration that you want to delete.
dqlcId :: Lens' DeleteQueryLoggingConfig Text
dqlcId = lens _dqlcId (\ s a -> s{_dqlcId = a})

instance AWSRequest DeleteQueryLoggingConfig where
        type Rs DeleteQueryLoggingConfig =
             DeleteQueryLoggingConfigResponse
        request = delete route53
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteQueryLoggingConfigResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteQueryLoggingConfig where

instance NFData DeleteQueryLoggingConfig where

instance ToHeaders DeleteQueryLoggingConfig where
        toHeaders = const mempty

instance ToPath DeleteQueryLoggingConfig where
        toPath DeleteQueryLoggingConfig'{..}
          = mconcat
              ["/2013-04-01/queryloggingconfig/", toBS _dqlcId]

instance ToQuery DeleteQueryLoggingConfig where
        toQuery = const mempty

-- | /See:/ 'deleteQueryLoggingConfigResponse' smart constructor.
newtype DeleteQueryLoggingConfigResponse = DeleteQueryLoggingConfigResponse'
  { _dqlcrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteQueryLoggingConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqlcrsResponseStatus' - -- | The response status code.
deleteQueryLoggingConfigResponse
    :: Int -- ^ 'dqlcrsResponseStatus'
    -> DeleteQueryLoggingConfigResponse
deleteQueryLoggingConfigResponse pResponseStatus_ =
  DeleteQueryLoggingConfigResponse' {_dqlcrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dqlcrsResponseStatus :: Lens' DeleteQueryLoggingConfigResponse Int
dqlcrsResponseStatus = lens _dqlcrsResponseStatus (\ s a -> s{_dqlcrsResponseStatus = a})

instance NFData DeleteQueryLoggingConfigResponse
         where
