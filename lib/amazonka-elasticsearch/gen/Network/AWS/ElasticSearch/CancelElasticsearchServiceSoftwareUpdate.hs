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
-- Module      : Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a scheduled service software update for an Amazon ES domain. You can only perform this operation before the @AutomatedUpdateDate@ and when the @UpdateStatus@ is in the @PENDING_UPDATE@ state.
module Network.AWS.ElasticSearch.CancelElasticsearchServiceSoftwareUpdate
  ( -- * Creating a Request
    cancelElasticsearchServiceSoftwareUpdate,
    CancelElasticsearchServiceSoftwareUpdate,

    -- * Request Lenses
    cessuDomainName,

    -- * Destructuring the Response
    cancelElasticsearchServiceSoftwareUpdateResponse,
    CancelElasticsearchServiceSoftwareUpdateResponse,

    -- * Response Lenses
    cessursServiceSoftwareOptions,
    cessursResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'CancelElasticsearchServiceSoftwareUpdate' @ operation. Specifies the name of the Elasticsearch domain that you wish to cancel a service software update on.
--
--
--
-- /See:/ 'cancelElasticsearchServiceSoftwareUpdate' smart constructor.
newtype CancelElasticsearchServiceSoftwareUpdate = CancelElasticsearchServiceSoftwareUpdate'
  { _cessuDomainName ::
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

-- | Creates a value of 'CancelElasticsearchServiceSoftwareUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cessuDomainName' - The name of the domain that you want to stop the latest service software update on.
cancelElasticsearchServiceSoftwareUpdate ::
  -- | 'cessuDomainName'
  Text ->
  CancelElasticsearchServiceSoftwareUpdate
cancelElasticsearchServiceSoftwareUpdate pDomainName_ =
  CancelElasticsearchServiceSoftwareUpdate'
    { _cessuDomainName =
        pDomainName_
    }

-- | The name of the domain that you want to stop the latest service software update on.
cessuDomainName :: Lens' CancelElasticsearchServiceSoftwareUpdate Text
cessuDomainName = lens _cessuDomainName (\s a -> s {_cessuDomainName = a})

instance AWSRequest CancelElasticsearchServiceSoftwareUpdate where
  type
    Rs CancelElasticsearchServiceSoftwareUpdate =
      CancelElasticsearchServiceSoftwareUpdateResponse
  request = postJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          CancelElasticsearchServiceSoftwareUpdateResponse'
            <$> (x .?> "ServiceSoftwareOptions") <*> (pure (fromEnum s))
      )

instance Hashable CancelElasticsearchServiceSoftwareUpdate

instance NFData CancelElasticsearchServiceSoftwareUpdate

instance ToHeaders CancelElasticsearchServiceSoftwareUpdate where
  toHeaders = const mempty

instance ToJSON CancelElasticsearchServiceSoftwareUpdate where
  toJSON CancelElasticsearchServiceSoftwareUpdate' {..} =
    object (catMaybes [Just ("DomainName" .= _cessuDomainName)])

instance ToPath CancelElasticsearchServiceSoftwareUpdate where
  toPath = const "/2015-01-01/es/serviceSoftwareUpdate/cancel"

instance ToQuery CancelElasticsearchServiceSoftwareUpdate where
  toQuery = const mempty

-- | The result of a @CancelElasticsearchServiceSoftwareUpdate@ operation. Contains the status of the update.
--
--
--
-- /See:/ 'cancelElasticsearchServiceSoftwareUpdateResponse' smart constructor.
data CancelElasticsearchServiceSoftwareUpdateResponse = CancelElasticsearchServiceSoftwareUpdateResponse'
  { _cessursServiceSoftwareOptions ::
      !( Maybe
           ServiceSoftwareOptions
       ),
    _cessursResponseStatus ::
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

-- | Creates a value of 'CancelElasticsearchServiceSoftwareUpdateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cessursServiceSoftwareOptions' - The current status of the Elasticsearch service software update.
--
-- * 'cessursResponseStatus' - -- | The response status code.
cancelElasticsearchServiceSoftwareUpdateResponse ::
  -- | 'cessursResponseStatus'
  Int ->
  CancelElasticsearchServiceSoftwareUpdateResponse
cancelElasticsearchServiceSoftwareUpdateResponse pResponseStatus_ =
  CancelElasticsearchServiceSoftwareUpdateResponse'
    { _cessursServiceSoftwareOptions =
        Nothing,
      _cessursResponseStatus = pResponseStatus_
    }

-- | The current status of the Elasticsearch service software update.
cessursServiceSoftwareOptions :: Lens' CancelElasticsearchServiceSoftwareUpdateResponse (Maybe ServiceSoftwareOptions)
cessursServiceSoftwareOptions = lens _cessursServiceSoftwareOptions (\s a -> s {_cessursServiceSoftwareOptions = a})

-- | -- | The response status code.
cessursResponseStatus :: Lens' CancelElasticsearchServiceSoftwareUpdateResponse Int
cessursResponseStatus = lens _cessursResponseStatus (\s a -> s {_cessursResponseStatus = a})

instance NFData CancelElasticsearchServiceSoftwareUpdateResponse
