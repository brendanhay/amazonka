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
-- Module      : Network.AWS.IoT.GetIndexingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the indexing configuration.
module Network.AWS.IoT.GetIndexingConfiguration
  ( -- * Creating a Request
    GetIndexingConfiguration (..),
    newGetIndexingConfiguration,

    -- * Destructuring the Response
    GetIndexingConfigurationResponse (..),
    newGetIndexingConfigurationResponse,

    -- * Response Lenses
    getIndexingConfigurationResponse_thingGroupIndexingConfiguration,
    getIndexingConfigurationResponse_thingIndexingConfiguration,
    getIndexingConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetIndexingConfiguration' smart constructor.
data GetIndexingConfiguration = GetIndexingConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIndexingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetIndexingConfiguration ::
  GetIndexingConfiguration
newGetIndexingConfiguration =
  GetIndexingConfiguration'

instance Core.AWSRequest GetIndexingConfiguration where
  type
    AWSResponse GetIndexingConfiguration =
      GetIndexingConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIndexingConfigurationResponse'
            Prelude.<$> (x Core..?> "thingGroupIndexingConfiguration")
            Prelude.<*> (x Core..?> "thingIndexingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIndexingConfiguration

instance Prelude.NFData GetIndexingConfiguration

instance Core.ToHeaders GetIndexingConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetIndexingConfiguration where
  toPath = Prelude.const "/indexing/config"

instance Core.ToQuery GetIndexingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIndexingConfigurationResponse' smart constructor.
data GetIndexingConfigurationResponse = GetIndexingConfigurationResponse'
  { -- | The index configuration.
    thingGroupIndexingConfiguration :: Prelude.Maybe ThingGroupIndexingConfiguration,
    -- | Thing indexing configuration.
    thingIndexingConfiguration :: Prelude.Maybe ThingIndexingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIndexingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingGroupIndexingConfiguration', 'getIndexingConfigurationResponse_thingGroupIndexingConfiguration' - The index configuration.
--
-- 'thingIndexingConfiguration', 'getIndexingConfigurationResponse_thingIndexingConfiguration' - Thing indexing configuration.
--
-- 'httpStatus', 'getIndexingConfigurationResponse_httpStatus' - The response's http status code.
newGetIndexingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIndexingConfigurationResponse
newGetIndexingConfigurationResponse pHttpStatus_ =
  GetIndexingConfigurationResponse'
    { thingGroupIndexingConfiguration =
        Prelude.Nothing,
      thingIndexingConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The index configuration.
getIndexingConfigurationResponse_thingGroupIndexingConfiguration :: Lens.Lens' GetIndexingConfigurationResponse (Prelude.Maybe ThingGroupIndexingConfiguration)
getIndexingConfigurationResponse_thingGroupIndexingConfiguration = Lens.lens (\GetIndexingConfigurationResponse' {thingGroupIndexingConfiguration} -> thingGroupIndexingConfiguration) (\s@GetIndexingConfigurationResponse' {} a -> s {thingGroupIndexingConfiguration = a} :: GetIndexingConfigurationResponse)

-- | Thing indexing configuration.
getIndexingConfigurationResponse_thingIndexingConfiguration :: Lens.Lens' GetIndexingConfigurationResponse (Prelude.Maybe ThingIndexingConfiguration)
getIndexingConfigurationResponse_thingIndexingConfiguration = Lens.lens (\GetIndexingConfigurationResponse' {thingIndexingConfiguration} -> thingIndexingConfiguration) (\s@GetIndexingConfigurationResponse' {} a -> s {thingIndexingConfiguration = a} :: GetIndexingConfigurationResponse)

-- | The response's http status code.
getIndexingConfigurationResponse_httpStatus :: Lens.Lens' GetIndexingConfigurationResponse Prelude.Int
getIndexingConfigurationResponse_httpStatus = Lens.lens (\GetIndexingConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetIndexingConfigurationResponse' {} a -> s {httpStatus = a} :: GetIndexingConfigurationResponse)

instance
  Prelude.NFData
    GetIndexingConfigurationResponse
