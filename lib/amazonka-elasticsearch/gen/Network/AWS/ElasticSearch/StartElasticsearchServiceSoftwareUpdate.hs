{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Schedules a service software update for an Amazon ES domain.
module Network.AWS.ElasticSearch.StartElasticsearchServiceSoftwareUpdate
  ( -- * Creating a Request
    startElasticsearchServiceSoftwareUpdate,
    StartElasticsearchServiceSoftwareUpdate,

    -- * Request Lenses
    sessuDomainName,

    -- * Destructuring the Response
    startElasticsearchServiceSoftwareUpdateResponse,
    StartElasticsearchServiceSoftwareUpdateResponse,

    -- * Response Lenses
    sessursServiceSoftwareOptions,
    sessursResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'StartElasticsearchServiceSoftwareUpdate' @ operation. Specifies the name of the Elasticsearch domain that you wish to schedule a service software update on.
--
--
--
-- /See:/ 'startElasticsearchServiceSoftwareUpdate' smart constructor.
newtype StartElasticsearchServiceSoftwareUpdate = StartElasticsearchServiceSoftwareUpdate'
  { _sessuDomainName ::
      Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartElasticsearchServiceSoftwareUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sessuDomainName' - The name of the domain that you want to update to the latest service software.
startElasticsearchServiceSoftwareUpdate ::
  -- | 'sessuDomainName'
  Text ->
  StartElasticsearchServiceSoftwareUpdate
startElasticsearchServiceSoftwareUpdate pDomainName_ =
  StartElasticsearchServiceSoftwareUpdate'
    { _sessuDomainName =
        pDomainName_
    }

-- | The name of the domain that you want to update to the latest service software.
sessuDomainName :: Lens' StartElasticsearchServiceSoftwareUpdate Text
sessuDomainName = lens _sessuDomainName (\s a -> s {_sessuDomainName = a})

instance AWSRequest StartElasticsearchServiceSoftwareUpdate where
  type
    Rs StartElasticsearchServiceSoftwareUpdate =
      StartElasticsearchServiceSoftwareUpdateResponse
  request = postJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          StartElasticsearchServiceSoftwareUpdateResponse'
            <$> (x .?> "ServiceSoftwareOptions") <*> (pure (fromEnum s))
      )

instance Hashable StartElasticsearchServiceSoftwareUpdate

instance NFData StartElasticsearchServiceSoftwareUpdate

instance ToHeaders StartElasticsearchServiceSoftwareUpdate where
  toHeaders = const mempty

instance ToJSON StartElasticsearchServiceSoftwareUpdate where
  toJSON StartElasticsearchServiceSoftwareUpdate' {..} =
    object (catMaybes [Just ("DomainName" .= _sessuDomainName)])

instance ToPath StartElasticsearchServiceSoftwareUpdate where
  toPath = const "/2015-01-01/es/serviceSoftwareUpdate/start"

instance ToQuery StartElasticsearchServiceSoftwareUpdate where
  toQuery = const mempty

-- | The result of a @StartElasticsearchServiceSoftwareUpdate@ operation. Contains the status of the update.
--
--
--
-- /See:/ 'startElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data StartElasticsearchServiceSoftwareUpdateResponse = StartElasticsearchServiceSoftwareUpdateResponse'
  { _sessursServiceSoftwareOptions ::
      !( Maybe
           ServiceSoftwareOptions
       ),
    _sessursResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'StartElasticsearchServiceSoftwareUpdateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sessursServiceSoftwareOptions' - The current status of the Elasticsearch service software update.
--
-- * 'sessursResponseStatus' - -- | The response status code.
startElasticsearchServiceSoftwareUpdateResponse ::
  -- | 'sessursResponseStatus'
  Int ->
  StartElasticsearchServiceSoftwareUpdateResponse
startElasticsearchServiceSoftwareUpdateResponse pResponseStatus_ =
  StartElasticsearchServiceSoftwareUpdateResponse'
    { _sessursServiceSoftwareOptions =
        Nothing,
      _sessursResponseStatus = pResponseStatus_
    }

-- | The current status of the Elasticsearch service software update.
sessursServiceSoftwareOptions :: Lens' StartElasticsearchServiceSoftwareUpdateResponse (Maybe ServiceSoftwareOptions)
sessursServiceSoftwareOptions = lens _sessursServiceSoftwareOptions (\s a -> s {_sessursServiceSoftwareOptions = a})

-- | -- | The response status code.
sessursResponseStatus :: Lens' StartElasticsearchServiceSoftwareUpdateResponse Int
sessursResponseStatus = lens _sessursResponseStatus (\s a -> s {_sessursResponseStatus = a})

instance NFData StartElasticsearchServiceSoftwareUpdateResponse
