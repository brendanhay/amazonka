{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AnalyticsMetadataType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An Amazon Pinpoint analytics endpoint.
--
-- An endpoint uniquely identifies a mobile device, email address, or phone
-- number that can receive messages from Amazon Pinpoint analytics.
--
-- Cognito User Pools only supports sending events to Amazon Pinpoint
-- projects in the US East (N. Virginia) us-east-1 Region, regardless of
-- the region in which the user pool resides.
--
-- /See:/ 'newAnalyticsMetadataType' smart constructor.
data AnalyticsMetadataType = AnalyticsMetadataType'
  { -- | The endpoint ID.
    analyticsEndpointId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AnalyticsMetadataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsEndpointId', 'analyticsMetadataType_analyticsEndpointId' - The endpoint ID.
newAnalyticsMetadataType ::
  AnalyticsMetadataType
newAnalyticsMetadataType =
  AnalyticsMetadataType'
    { analyticsEndpointId =
        Core.Nothing
    }

-- | The endpoint ID.
analyticsMetadataType_analyticsEndpointId :: Lens.Lens' AnalyticsMetadataType (Core.Maybe Core.Text)
analyticsMetadataType_analyticsEndpointId = Lens.lens (\AnalyticsMetadataType' {analyticsEndpointId} -> analyticsEndpointId) (\s@AnalyticsMetadataType' {} a -> s {analyticsEndpointId = a} :: AnalyticsMetadataType)

instance Core.Hashable AnalyticsMetadataType

instance Core.NFData AnalyticsMetadataType

instance Core.ToJSON AnalyticsMetadataType where
  toJSON AnalyticsMetadataType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AnalyticsEndpointId" Core..=)
              Core.<$> analyticsEndpointId
          ]
      )
