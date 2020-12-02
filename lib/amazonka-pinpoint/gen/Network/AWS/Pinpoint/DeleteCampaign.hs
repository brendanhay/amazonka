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
-- Module      : Network.AWS.Pinpoint.DeleteCampaign
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a campaign from an application.
module Network.AWS.Pinpoint.DeleteCampaign
  ( -- * Creating a Request
    deleteCampaign,
    DeleteCampaign,

    -- * Request Lenses
    dcCampaignId,
    dcApplicationId,

    -- * Destructuring the Response
    deleteCampaignResponse,
    DeleteCampaignResponse,

    -- * Response Lenses
    dcrsResponseStatus,
    dcrsCampaignResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCampaign' smart constructor.
data DeleteCampaign = DeleteCampaign'
  { _dcCampaignId :: !Text,
    _dcApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCampaign' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcCampaignId' - The unique identifier for the campaign.
--
-- * 'dcApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
deleteCampaign ::
  -- | 'dcCampaignId'
  Text ->
  -- | 'dcApplicationId'
  Text ->
  DeleteCampaign
deleteCampaign pCampaignId_ pApplicationId_ =
  DeleteCampaign'
    { _dcCampaignId = pCampaignId_,
      _dcApplicationId = pApplicationId_
    }

-- | The unique identifier for the campaign.
dcCampaignId :: Lens' DeleteCampaign Text
dcCampaignId = lens _dcCampaignId (\s a -> s {_dcCampaignId = a})

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
dcApplicationId :: Lens' DeleteCampaign Text
dcApplicationId = lens _dcApplicationId (\s a -> s {_dcApplicationId = a})

instance AWSRequest DeleteCampaign where
  type Rs DeleteCampaign = DeleteCampaignResponse
  request = delete pinpoint
  response =
    receiveJSON
      ( \s h x ->
          DeleteCampaignResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable DeleteCampaign

instance NFData DeleteCampaign

instance ToHeaders DeleteCampaign where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteCampaign where
  toPath DeleteCampaign' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _dcApplicationId,
        "/campaigns/",
        toBS _dcCampaignId
      ]

instance ToQuery DeleteCampaign where
  toQuery = const mempty

-- | /See:/ 'deleteCampaignResponse' smart constructor.
data DeleteCampaignResponse = DeleteCampaignResponse'
  { _dcrsResponseStatus ::
      !Int,
    _dcrsCampaignResponse :: !CampaignResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteCampaignResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsResponseStatus' - -- | The response status code.
--
-- * 'dcrsCampaignResponse' - Undocumented member.
deleteCampaignResponse ::
  -- | 'dcrsResponseStatus'
  Int ->
  -- | 'dcrsCampaignResponse'
  CampaignResponse ->
  DeleteCampaignResponse
deleteCampaignResponse pResponseStatus_ pCampaignResponse_ =
  DeleteCampaignResponse'
    { _dcrsResponseStatus = pResponseStatus_,
      _dcrsCampaignResponse = pCampaignResponse_
    }

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DeleteCampaignResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\s a -> s {_dcrsResponseStatus = a})

-- | Undocumented member.
dcrsCampaignResponse :: Lens' DeleteCampaignResponse CampaignResponse
dcrsCampaignResponse = lens _dcrsCampaignResponse (\s a -> s {_dcrsCampaignResponse = a})

instance NFData DeleteCampaignResponse
