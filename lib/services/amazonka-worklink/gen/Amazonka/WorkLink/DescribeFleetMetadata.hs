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
-- Module      : Amazonka.WorkLink.DescribeFleetMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides basic information for the specified fleet, excluding identity
-- provider, networking, and device configuration details.
module Amazonka.WorkLink.DescribeFleetMetadata
  ( -- * Creating a Request
    DescribeFleetMetadata (..),
    newDescribeFleetMetadata,

    -- * Request Lenses
    describeFleetMetadata_fleetArn,

    -- * Destructuring the Response
    DescribeFleetMetadataResponse (..),
    newDescribeFleetMetadataResponse,

    -- * Response Lenses
    describeFleetMetadataResponse_tags,
    describeFleetMetadataResponse_companyCode,
    describeFleetMetadataResponse_createdTime,
    describeFleetMetadataResponse_displayName,
    describeFleetMetadataResponse_lastUpdatedTime,
    describeFleetMetadataResponse_fleetName,
    describeFleetMetadataResponse_optimizeForEndUserLocation,
    describeFleetMetadataResponse_fleetStatus,
    describeFleetMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newDescribeFleetMetadata' smart constructor.
data DescribeFleetMetadata = DescribeFleetMetadata'
  { -- | The Amazon Resource Name (ARN) of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'describeFleetMetadata_fleetArn' - The Amazon Resource Name (ARN) of the fleet.
newDescribeFleetMetadata ::
  -- | 'fleetArn'
  Prelude.Text ->
  DescribeFleetMetadata
newDescribeFleetMetadata pFleetArn_ =
  DescribeFleetMetadata' {fleetArn = pFleetArn_}

-- | The Amazon Resource Name (ARN) of the fleet.
describeFleetMetadata_fleetArn :: Lens.Lens' DescribeFleetMetadata Prelude.Text
describeFleetMetadata_fleetArn = Lens.lens (\DescribeFleetMetadata' {fleetArn} -> fleetArn) (\s@DescribeFleetMetadata' {} a -> s {fleetArn = a} :: DescribeFleetMetadata)

instance Core.AWSRequest DescribeFleetMetadata where
  type
    AWSResponse DescribeFleetMetadata =
      DescribeFleetMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFleetMetadataResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "CompanyCode")
            Prelude.<*> (x Core..?> "CreatedTime")
            Prelude.<*> (x Core..?> "DisplayName")
            Prelude.<*> (x Core..?> "LastUpdatedTime")
            Prelude.<*> (x Core..?> "FleetName")
            Prelude.<*> (x Core..?> "OptimizeForEndUserLocation")
            Prelude.<*> (x Core..?> "FleetStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFleetMetadata where
  hashWithSalt _salt DescribeFleetMetadata' {..} =
    _salt `Prelude.hashWithSalt` fleetArn

instance Prelude.NFData DescribeFleetMetadata where
  rnf DescribeFleetMetadata' {..} = Prelude.rnf fleetArn

instance Core.ToHeaders DescribeFleetMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeFleetMetadata where
  toJSON DescribeFleetMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("FleetArn" Core..= fleetArn)]
      )

instance Core.ToPath DescribeFleetMetadata where
  toPath = Prelude.const "/describeFleetMetadata"

instance Core.ToQuery DescribeFleetMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFleetMetadataResponse' smart constructor.
data DescribeFleetMetadataResponse = DescribeFleetMetadataResponse'
  { -- | The tags attached to the resource. A tag is a key-value pair.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier used by users to sign in to the Amazon WorkLink app.
    companyCode :: Prelude.Maybe Prelude.Text,
    -- | The time that the fleet was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The time that the fleet was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the fleet.
    fleetName :: Prelude.Maybe Prelude.Text,
    -- | The option to optimize for better performance by routing traffic through
    -- the closest AWS Region to users, which may be outside of your home
    -- Region.
    optimizeForEndUserLocation :: Prelude.Maybe Prelude.Bool,
    -- | The current state of the fleet.
    fleetStatus :: Prelude.Maybe FleetStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFleetMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeFleetMetadataResponse_tags' - The tags attached to the resource. A tag is a key-value pair.
--
-- 'companyCode', 'describeFleetMetadataResponse_companyCode' - The identifier used by users to sign in to the Amazon WorkLink app.
--
-- 'createdTime', 'describeFleetMetadataResponse_createdTime' - The time that the fleet was created.
--
-- 'displayName', 'describeFleetMetadataResponse_displayName' - The name to display.
--
-- 'lastUpdatedTime', 'describeFleetMetadataResponse_lastUpdatedTime' - The time that the fleet was last updated.
--
-- 'fleetName', 'describeFleetMetadataResponse_fleetName' - The name of the fleet.
--
-- 'optimizeForEndUserLocation', 'describeFleetMetadataResponse_optimizeForEndUserLocation' - The option to optimize for better performance by routing traffic through
-- the closest AWS Region to users, which may be outside of your home
-- Region.
--
-- 'fleetStatus', 'describeFleetMetadataResponse_fleetStatus' - The current state of the fleet.
--
-- 'httpStatus', 'describeFleetMetadataResponse_httpStatus' - The response's http status code.
newDescribeFleetMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeFleetMetadataResponse
newDescribeFleetMetadataResponse pHttpStatus_ =
  DescribeFleetMetadataResponse'
    { tags =
        Prelude.Nothing,
      companyCode = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      fleetName = Prelude.Nothing,
      optimizeForEndUserLocation = Prelude.Nothing,
      fleetStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags attached to the resource. A tag is a key-value pair.
describeFleetMetadataResponse_tags :: Lens.Lens' DescribeFleetMetadataResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeFleetMetadataResponse_tags = Lens.lens (\DescribeFleetMetadataResponse' {tags} -> tags) (\s@DescribeFleetMetadataResponse' {} a -> s {tags = a} :: DescribeFleetMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier used by users to sign in to the Amazon WorkLink app.
describeFleetMetadataResponse_companyCode :: Lens.Lens' DescribeFleetMetadataResponse (Prelude.Maybe Prelude.Text)
describeFleetMetadataResponse_companyCode = Lens.lens (\DescribeFleetMetadataResponse' {companyCode} -> companyCode) (\s@DescribeFleetMetadataResponse' {} a -> s {companyCode = a} :: DescribeFleetMetadataResponse)

-- | The time that the fleet was created.
describeFleetMetadataResponse_createdTime :: Lens.Lens' DescribeFleetMetadataResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetMetadataResponse_createdTime = Lens.lens (\DescribeFleetMetadataResponse' {createdTime} -> createdTime) (\s@DescribeFleetMetadataResponse' {} a -> s {createdTime = a} :: DescribeFleetMetadataResponse) Prelude.. Lens.mapping Core._Time

-- | The name to display.
describeFleetMetadataResponse_displayName :: Lens.Lens' DescribeFleetMetadataResponse (Prelude.Maybe Prelude.Text)
describeFleetMetadataResponse_displayName = Lens.lens (\DescribeFleetMetadataResponse' {displayName} -> displayName) (\s@DescribeFleetMetadataResponse' {} a -> s {displayName = a} :: DescribeFleetMetadataResponse)

-- | The time that the fleet was last updated.
describeFleetMetadataResponse_lastUpdatedTime :: Lens.Lens' DescribeFleetMetadataResponse (Prelude.Maybe Prelude.UTCTime)
describeFleetMetadataResponse_lastUpdatedTime = Lens.lens (\DescribeFleetMetadataResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@DescribeFleetMetadataResponse' {} a -> s {lastUpdatedTime = a} :: DescribeFleetMetadataResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the fleet.
describeFleetMetadataResponse_fleetName :: Lens.Lens' DescribeFleetMetadataResponse (Prelude.Maybe Prelude.Text)
describeFleetMetadataResponse_fleetName = Lens.lens (\DescribeFleetMetadataResponse' {fleetName} -> fleetName) (\s@DescribeFleetMetadataResponse' {} a -> s {fleetName = a} :: DescribeFleetMetadataResponse)

-- | The option to optimize for better performance by routing traffic through
-- the closest AWS Region to users, which may be outside of your home
-- Region.
describeFleetMetadataResponse_optimizeForEndUserLocation :: Lens.Lens' DescribeFleetMetadataResponse (Prelude.Maybe Prelude.Bool)
describeFleetMetadataResponse_optimizeForEndUserLocation = Lens.lens (\DescribeFleetMetadataResponse' {optimizeForEndUserLocation} -> optimizeForEndUserLocation) (\s@DescribeFleetMetadataResponse' {} a -> s {optimizeForEndUserLocation = a} :: DescribeFleetMetadataResponse)

-- | The current state of the fleet.
describeFleetMetadataResponse_fleetStatus :: Lens.Lens' DescribeFleetMetadataResponse (Prelude.Maybe FleetStatus)
describeFleetMetadataResponse_fleetStatus = Lens.lens (\DescribeFleetMetadataResponse' {fleetStatus} -> fleetStatus) (\s@DescribeFleetMetadataResponse' {} a -> s {fleetStatus = a} :: DescribeFleetMetadataResponse)

-- | The response's http status code.
describeFleetMetadataResponse_httpStatus :: Lens.Lens' DescribeFleetMetadataResponse Prelude.Int
describeFleetMetadataResponse_httpStatus = Lens.lens (\DescribeFleetMetadataResponse' {httpStatus} -> httpStatus) (\s@DescribeFleetMetadataResponse' {} a -> s {httpStatus = a} :: DescribeFleetMetadataResponse)

instance Prelude.NFData DescribeFleetMetadataResponse where
  rnf DescribeFleetMetadataResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf companyCode
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf optimizeForEndUserLocation
      `Prelude.seq` Prelude.rnf fleetStatus
      `Prelude.seq` Prelude.rnf httpStatus
