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
-- Module      : Amazonka.Backup.DescribeRegionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current service opt-in settings for the Region. If service
-- opt-in is enabled for a service, Backup tries to protect that service\'s
-- resources in this Region, when the resource is included in an on-demand
-- backup or scheduled backup plan. Otherwise, Backup does not try to
-- protect that service\'s resources in this Region.
module Amazonka.Backup.DescribeRegionSettings
  ( -- * Creating a Request
    DescribeRegionSettings (..),
    newDescribeRegionSettings,

    -- * Destructuring the Response
    DescribeRegionSettingsResponse (..),
    newDescribeRegionSettingsResponse,

    -- * Response Lenses
    describeRegionSettingsResponse_resourceTypeOptInPreference,
    describeRegionSettingsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRegionSettings' smart constructor.
data DescribeRegionSettings = DescribeRegionSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRegionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeRegionSettings ::
  DescribeRegionSettings
newDescribeRegionSettings = DescribeRegionSettings'

instance Core.AWSRequest DescribeRegionSettings where
  type
    AWSResponse DescribeRegionSettings =
      DescribeRegionSettingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRegionSettingsResponse'
            Prelude.<$> ( x Core..?> "ResourceTypeOptInPreference"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRegionSettings where
  hashWithSalt salt' _ =
    salt' `Prelude.hashWithSalt` (0 :: Prelude.Int)

instance Prelude.NFData DescribeRegionSettings where
  rnf _ = ()

instance Core.ToHeaders DescribeRegionSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeRegionSettings where
  toPath = Prelude.const "/account-settings"

instance Core.ToQuery DescribeRegionSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRegionSettingsResponse' smart constructor.
data DescribeRegionSettingsResponse = DescribeRegionSettingsResponse'
  { -- | Returns a list of all services along with the opt-in preferences in the
    -- Region.
    resourceTypeOptInPreference :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRegionSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTypeOptInPreference', 'describeRegionSettingsResponse_resourceTypeOptInPreference' - Returns a list of all services along with the opt-in preferences in the
-- Region.
--
-- 'httpStatus', 'describeRegionSettingsResponse_httpStatus' - The response's http status code.
newDescribeRegionSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRegionSettingsResponse
newDescribeRegionSettingsResponse pHttpStatus_ =
  DescribeRegionSettingsResponse'
    { resourceTypeOptInPreference =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of all services along with the opt-in preferences in the
-- Region.
describeRegionSettingsResponse_resourceTypeOptInPreference :: Lens.Lens' DescribeRegionSettingsResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool))
describeRegionSettingsResponse_resourceTypeOptInPreference = Lens.lens (\DescribeRegionSettingsResponse' {resourceTypeOptInPreference} -> resourceTypeOptInPreference) (\s@DescribeRegionSettingsResponse' {} a -> s {resourceTypeOptInPreference = a} :: DescribeRegionSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeRegionSettingsResponse_httpStatus :: Lens.Lens' DescribeRegionSettingsResponse Prelude.Int
describeRegionSettingsResponse_httpStatus = Lens.lens (\DescribeRegionSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeRegionSettingsResponse' {} a -> s {httpStatus = a} :: DescribeRegionSettingsResponse)

instance
  Prelude.NFData
    DescribeRegionSettingsResponse
  where
  rnf DescribeRegionSettingsResponse' {..} =
    Prelude.rnf resourceTypeOptInPreference
      `Prelude.seq` Prelude.rnf httpStatus
