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
-- Module      : Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all provisioning artifacts (also known as versions) for the specified self-service action.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ServiceCatalog.ListProvisioningArtifactsForServiceAction
  ( -- * Creating a Request
    listProvisioningArtifactsForServiceAction,
    ListProvisioningArtifactsForServiceAction,

    -- * Request Lenses
    lpafsaAcceptLanguage,
    lpafsaPageToken,
    lpafsaPageSize,
    lpafsaServiceActionId,

    -- * Destructuring the Response
    listProvisioningArtifactsForServiceActionResponse,
    ListProvisioningArtifactsForServiceActionResponse,

    -- * Response Lenses
    lpafsarsNextPageToken,
    lpafsarsProvisioningArtifactViews,
    lpafsarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'listProvisioningArtifactsForServiceAction' smart constructor.
data ListProvisioningArtifactsForServiceAction = ListProvisioningArtifactsForServiceAction'
  { _lpafsaAcceptLanguage ::
      !( Maybe
           Text
       ),
    _lpafsaPageToken ::
      !( Maybe
           Text
       ),
    _lpafsaPageSize ::
      !( Maybe
           Nat
       ),
    _lpafsaServiceActionId ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'ListProvisioningArtifactsForServiceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpafsaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'lpafsaPageToken' - The page token for the next set of results. To retrieve the first set of results, use null.
--
-- * 'lpafsaPageSize' - The maximum number of items to return with this call.
--
-- * 'lpafsaServiceActionId' - The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
listProvisioningArtifactsForServiceAction ::
  -- | 'lpafsaServiceActionId'
  Text ->
  ListProvisioningArtifactsForServiceAction
listProvisioningArtifactsForServiceAction pServiceActionId_ =
  ListProvisioningArtifactsForServiceAction'
    { _lpafsaAcceptLanguage =
        Nothing,
      _lpafsaPageToken = Nothing,
      _lpafsaPageSize = Nothing,
      _lpafsaServiceActionId = pServiceActionId_
    }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
lpafsaAcceptLanguage :: Lens' ListProvisioningArtifactsForServiceAction (Maybe Text)
lpafsaAcceptLanguage = lens _lpafsaAcceptLanguage (\s a -> s {_lpafsaAcceptLanguage = a})

-- | The page token for the next set of results. To retrieve the first set of results, use null.
lpafsaPageToken :: Lens' ListProvisioningArtifactsForServiceAction (Maybe Text)
lpafsaPageToken = lens _lpafsaPageToken (\s a -> s {_lpafsaPageToken = a})

-- | The maximum number of items to return with this call.
lpafsaPageSize :: Lens' ListProvisioningArtifactsForServiceAction (Maybe Natural)
lpafsaPageSize = lens _lpafsaPageSize (\s a -> s {_lpafsaPageSize = a}) . mapping _Nat

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@ .
lpafsaServiceActionId :: Lens' ListProvisioningArtifactsForServiceAction Text
lpafsaServiceActionId = lens _lpafsaServiceActionId (\s a -> s {_lpafsaServiceActionId = a})

instance AWSPager ListProvisioningArtifactsForServiceAction where
  page rq rs
    | stop (rs ^. lpafsarsNextPageToken) = Nothing
    | stop (rs ^. lpafsarsProvisioningArtifactViews) = Nothing
    | otherwise =
      Just $ rq & lpafsaPageToken .~ rs ^. lpafsarsNextPageToken

instance AWSRequest ListProvisioningArtifactsForServiceAction where
  type
    Rs ListProvisioningArtifactsForServiceAction =
      ListProvisioningArtifactsForServiceActionResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          ListProvisioningArtifactsForServiceActionResponse'
            <$> (x .?> "NextPageToken")
            <*> (x .?> "ProvisioningArtifactViews" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListProvisioningArtifactsForServiceAction

instance NFData ListProvisioningArtifactsForServiceAction

instance ToHeaders ListProvisioningArtifactsForServiceAction where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.ListProvisioningArtifactsForServiceAction" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListProvisioningArtifactsForServiceAction where
  toJSON ListProvisioningArtifactsForServiceAction' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _lpafsaAcceptLanguage,
            ("PageToken" .=) <$> _lpafsaPageToken,
            ("PageSize" .=) <$> _lpafsaPageSize,
            Just ("ServiceActionId" .= _lpafsaServiceActionId)
          ]
      )

instance ToPath ListProvisioningArtifactsForServiceAction where
  toPath = const "/"

instance ToQuery ListProvisioningArtifactsForServiceAction where
  toQuery = const mempty

-- | /See:/ 'listProvisioningArtifactsForServiceActionResponse' smart constructor.
data ListProvisioningArtifactsForServiceActionResponse = ListProvisioningArtifactsForServiceActionResponse'
  { _lpafsarsNextPageToken ::
      !( Maybe
           Text
       ),
    _lpafsarsProvisioningArtifactViews ::
      !( Maybe
           [ProvisioningArtifactView]
       ),
    _lpafsarsResponseStatus ::
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

-- | Creates a value of 'ListProvisioningArtifactsForServiceActionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpafsarsNextPageToken' - The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
--
-- * 'lpafsarsProvisioningArtifactViews' - An array of objects with information about product views and provisioning artifacts.
--
-- * 'lpafsarsResponseStatus' - -- | The response status code.
listProvisioningArtifactsForServiceActionResponse ::
  -- | 'lpafsarsResponseStatus'
  Int ->
  ListProvisioningArtifactsForServiceActionResponse
listProvisioningArtifactsForServiceActionResponse pResponseStatus_ =
  ListProvisioningArtifactsForServiceActionResponse'
    { _lpafsarsNextPageToken =
        Nothing,
      _lpafsarsProvisioningArtifactViews = Nothing,
      _lpafsarsResponseStatus = pResponseStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are no additional results, this value is null.
lpafsarsNextPageToken :: Lens' ListProvisioningArtifactsForServiceActionResponse (Maybe Text)
lpafsarsNextPageToken = lens _lpafsarsNextPageToken (\s a -> s {_lpafsarsNextPageToken = a})

-- | An array of objects with information about product views and provisioning artifacts.
lpafsarsProvisioningArtifactViews :: Lens' ListProvisioningArtifactsForServiceActionResponse [ProvisioningArtifactView]
lpafsarsProvisioningArtifactViews = lens _lpafsarsProvisioningArtifactViews (\s a -> s {_lpafsarsProvisioningArtifactViews = a}) . _Default . _Coerce

-- | -- | The response status code.
lpafsarsResponseStatus :: Lens' ListProvisioningArtifactsForServiceActionResponse Int
lpafsarsResponseStatus = lens _lpafsarsResponseStatus (\s a -> s {_lpafsarsResponseStatus = a})

instance NFData ListProvisioningArtifactsForServiceActionResponse
