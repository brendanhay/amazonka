{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distributions that have a cache behavior that’s
-- associated with the specified real-time log configuration.
--
-- You can specify the real-time log configuration by its name or its
-- Amazon Resource Name (ARN). You must provide at least one. If you
-- provide both, CloudFront uses the name to identify the real-time log
-- configuration to list distributions for.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
  ( -- * Creating a Request
    ListDistributionsByRealtimeLogConfig (..),
    newListDistributionsByRealtimeLogConfig,

    -- * Request Lenses
    listDistributionsByRealtimeLogConfig_realtimeLogConfigName,
    listDistributionsByRealtimeLogConfig_realtimeLogConfigArn,
    listDistributionsByRealtimeLogConfig_maxItems,
    listDistributionsByRealtimeLogConfig_marker,

    -- * Destructuring the Response
    ListDistributionsByRealtimeLogConfigResponse (..),
    newListDistributionsByRealtimeLogConfigResponse,

    -- * Response Lenses
    listDistributionsByRealtimeLogConfigResponse_distributionList,
    listDistributionsByRealtimeLogConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDistributionsByRealtimeLogConfig' smart constructor.
data ListDistributionsByRealtimeLogConfig = ListDistributionsByRealtimeLogConfig'
  { -- | The name of the real-time log configuration whose associated
    -- distributions you want to list.
    realtimeLogConfigName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration whose
    -- associated distributions you want to list.
    realtimeLogConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of distributions that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Text,
    -- | Use this field when paginating results to indicate where to begin in
    -- your list of distributions. The response includes distributions in the
    -- list that occur after the marker. To get the next page of the list, set
    -- this field’s value to the value of @NextMarker@ from the current page’s
    -- response.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByRealtimeLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realtimeLogConfigName', 'listDistributionsByRealtimeLogConfig_realtimeLogConfigName' - The name of the real-time log configuration whose associated
-- distributions you want to list.
--
-- 'realtimeLogConfigArn', 'listDistributionsByRealtimeLogConfig_realtimeLogConfigArn' - The Amazon Resource Name (ARN) of the real-time log configuration whose
-- associated distributions you want to list.
--
-- 'maxItems', 'listDistributionsByRealtimeLogConfig_maxItems' - The maximum number of distributions that you want in the response.
--
-- 'marker', 'listDistributionsByRealtimeLogConfig_marker' - Use this field when paginating results to indicate where to begin in
-- your list of distributions. The response includes distributions in the
-- list that occur after the marker. To get the next page of the list, set
-- this field’s value to the value of @NextMarker@ from the current page’s
-- response.
newListDistributionsByRealtimeLogConfig ::
  ListDistributionsByRealtimeLogConfig
newListDistributionsByRealtimeLogConfig =
  ListDistributionsByRealtimeLogConfig'
    { realtimeLogConfigName =
        Prelude.Nothing,
      realtimeLogConfigArn =
        Prelude.Nothing,
      maxItems = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The name of the real-time log configuration whose associated
-- distributions you want to list.
listDistributionsByRealtimeLogConfig_realtimeLogConfigName :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Prelude.Maybe Prelude.Text)
listDistributionsByRealtimeLogConfig_realtimeLogConfigName = Lens.lens (\ListDistributionsByRealtimeLogConfig' {realtimeLogConfigName} -> realtimeLogConfigName) (\s@ListDistributionsByRealtimeLogConfig' {} a -> s {realtimeLogConfigName = a} :: ListDistributionsByRealtimeLogConfig)

-- | The Amazon Resource Name (ARN) of the real-time log configuration whose
-- associated distributions you want to list.
listDistributionsByRealtimeLogConfig_realtimeLogConfigArn :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Prelude.Maybe Prelude.Text)
listDistributionsByRealtimeLogConfig_realtimeLogConfigArn = Lens.lens (\ListDistributionsByRealtimeLogConfig' {realtimeLogConfigArn} -> realtimeLogConfigArn) (\s@ListDistributionsByRealtimeLogConfig' {} a -> s {realtimeLogConfigArn = a} :: ListDistributionsByRealtimeLogConfig)

-- | The maximum number of distributions that you want in the response.
listDistributionsByRealtimeLogConfig_maxItems :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Prelude.Maybe Prelude.Text)
listDistributionsByRealtimeLogConfig_maxItems = Lens.lens (\ListDistributionsByRealtimeLogConfig' {maxItems} -> maxItems) (\s@ListDistributionsByRealtimeLogConfig' {} a -> s {maxItems = a} :: ListDistributionsByRealtimeLogConfig)

-- | Use this field when paginating results to indicate where to begin in
-- your list of distributions. The response includes distributions in the
-- list that occur after the marker. To get the next page of the list, set
-- this field’s value to the value of @NextMarker@ from the current page’s
-- response.
listDistributionsByRealtimeLogConfig_marker :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Prelude.Maybe Prelude.Text)
listDistributionsByRealtimeLogConfig_marker = Lens.lens (\ListDistributionsByRealtimeLogConfig' {marker} -> marker) (\s@ListDistributionsByRealtimeLogConfig' {} a -> s {marker = a} :: ListDistributionsByRealtimeLogConfig)

instance
  Core.AWSRequest
    ListDistributionsByRealtimeLogConfig
  where
  type
    AWSResponse ListDistributionsByRealtimeLogConfig =
      ListDistributionsByRealtimeLogConfigResponse
  request = Request.postXML defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByRealtimeLogConfigResponse'
            Prelude.<$> (Core.parseXML x)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDistributionsByRealtimeLogConfig

instance
  Prelude.NFData
    ListDistributionsByRealtimeLogConfig

instance
  Core.ToElement
    ListDistributionsByRealtimeLogConfig
  where
  toElement =
    Core.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}ListDistributionsByRealtimeLogConfigRequest"

instance
  Core.ToHeaders
    ListDistributionsByRealtimeLogConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ListDistributionsByRealtimeLogConfig
  where
  toPath =
    Prelude.const
      "/2020-05-31/distributionsByRealtimeLogConfig/"

instance
  Core.ToQuery
    ListDistributionsByRealtimeLogConfig
  where
  toQuery = Prelude.const Prelude.mempty

instance
  Core.ToXML
    ListDistributionsByRealtimeLogConfig
  where
  toXML ListDistributionsByRealtimeLogConfig' {..} =
    Prelude.mconcat
      [ "RealtimeLogConfigName"
          Core.@= realtimeLogConfigName,
        "RealtimeLogConfigArn" Core.@= realtimeLogConfigArn,
        "MaxItems" Core.@= maxItems,
        "Marker" Core.@= marker
      ]

-- | /See:/ 'newListDistributionsByRealtimeLogConfigResponse' smart constructor.
data ListDistributionsByRealtimeLogConfigResponse = ListDistributionsByRealtimeLogConfigResponse'
  { distributionList :: Prelude.Maybe DistributionList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionsByRealtimeLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionList', 'listDistributionsByRealtimeLogConfigResponse_distributionList' - Undocumented member.
--
-- 'httpStatus', 'listDistributionsByRealtimeLogConfigResponse_httpStatus' - The response's http status code.
newListDistributionsByRealtimeLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributionsByRealtimeLogConfigResponse
newListDistributionsByRealtimeLogConfigResponse
  pHttpStatus_ =
    ListDistributionsByRealtimeLogConfigResponse'
      { distributionList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
listDistributionsByRealtimeLogConfigResponse_distributionList :: Lens.Lens' ListDistributionsByRealtimeLogConfigResponse (Prelude.Maybe DistributionList)
listDistributionsByRealtimeLogConfigResponse_distributionList = Lens.lens (\ListDistributionsByRealtimeLogConfigResponse' {distributionList} -> distributionList) (\s@ListDistributionsByRealtimeLogConfigResponse' {} a -> s {distributionList = a} :: ListDistributionsByRealtimeLogConfigResponse)

-- | The response's http status code.
listDistributionsByRealtimeLogConfigResponse_httpStatus :: Lens.Lens' ListDistributionsByRealtimeLogConfigResponse Prelude.Int
listDistributionsByRealtimeLogConfigResponse_httpStatus = Lens.lens (\ListDistributionsByRealtimeLogConfigResponse' {httpStatus} -> httpStatus) (\s@ListDistributionsByRealtimeLogConfigResponse' {} a -> s {httpStatus = a} :: ListDistributionsByRealtimeLogConfigResponse)

instance
  Prelude.NFData
    ListDistributionsByRealtimeLogConfigResponse
