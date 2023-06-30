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
-- Module      : Amazonka.MigrationHubConfig.CreateHomeRegionControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API sets up the home region for the calling account only.
module Amazonka.MigrationHubConfig.CreateHomeRegionControl
  ( -- * Creating a Request
    CreateHomeRegionControl (..),
    newCreateHomeRegionControl,

    -- * Request Lenses
    createHomeRegionControl_dryRun,
    createHomeRegionControl_homeRegion,
    createHomeRegionControl_target,

    -- * Destructuring the Response
    CreateHomeRegionControlResponse (..),
    newCreateHomeRegionControlResponse,

    -- * Response Lenses
    createHomeRegionControlResponse_homeRegionControl,
    createHomeRegionControlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubConfig.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateHomeRegionControl' smart constructor.
data CreateHomeRegionControl = CreateHomeRegionControl'
  { -- | Optional Boolean flag to indicate whether any effect should take place.
    -- It tests whether the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the home region of the calling account.
    homeRegion :: Prelude.Text,
    -- | The account for which this command sets up a home region control. The
    -- @Target@ is always of type @ACCOUNT@.
    target :: Target
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHomeRegionControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createHomeRegionControl_dryRun' - Optional Boolean flag to indicate whether any effect should take place.
-- It tests whether the caller has permission to make the call.
--
-- 'homeRegion', 'createHomeRegionControl_homeRegion' - The name of the home region of the calling account.
--
-- 'target', 'createHomeRegionControl_target' - The account for which this command sets up a home region control. The
-- @Target@ is always of type @ACCOUNT@.
newCreateHomeRegionControl ::
  -- | 'homeRegion'
  Prelude.Text ->
  -- | 'target'
  Target ->
  CreateHomeRegionControl
newCreateHomeRegionControl pHomeRegion_ pTarget_ =
  CreateHomeRegionControl'
    { dryRun = Prelude.Nothing,
      homeRegion = pHomeRegion_,
      target = pTarget_
    }

-- | Optional Boolean flag to indicate whether any effect should take place.
-- It tests whether the caller has permission to make the call.
createHomeRegionControl_dryRun :: Lens.Lens' CreateHomeRegionControl (Prelude.Maybe Prelude.Bool)
createHomeRegionControl_dryRun = Lens.lens (\CreateHomeRegionControl' {dryRun} -> dryRun) (\s@CreateHomeRegionControl' {} a -> s {dryRun = a} :: CreateHomeRegionControl)

-- | The name of the home region of the calling account.
createHomeRegionControl_homeRegion :: Lens.Lens' CreateHomeRegionControl Prelude.Text
createHomeRegionControl_homeRegion = Lens.lens (\CreateHomeRegionControl' {homeRegion} -> homeRegion) (\s@CreateHomeRegionControl' {} a -> s {homeRegion = a} :: CreateHomeRegionControl)

-- | The account for which this command sets up a home region control. The
-- @Target@ is always of type @ACCOUNT@.
createHomeRegionControl_target :: Lens.Lens' CreateHomeRegionControl Target
createHomeRegionControl_target = Lens.lens (\CreateHomeRegionControl' {target} -> target) (\s@CreateHomeRegionControl' {} a -> s {target = a} :: CreateHomeRegionControl)

instance Core.AWSRequest CreateHomeRegionControl where
  type
    AWSResponse CreateHomeRegionControl =
      CreateHomeRegionControlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHomeRegionControlResponse'
            Prelude.<$> (x Data..?> "HomeRegionControl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHomeRegionControl where
  hashWithSalt _salt CreateHomeRegionControl' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` target

instance Prelude.NFData CreateHomeRegionControl where
  rnf CreateHomeRegionControl' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf target

instance Data.ToHeaders CreateHomeRegionControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMigrationHubMultiAccountService.CreateHomeRegionControl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHomeRegionControl where
  toJSON CreateHomeRegionControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DryRun" Data..=) Prelude.<$> dryRun,
            Prelude.Just ("HomeRegion" Data..= homeRegion),
            Prelude.Just ("Target" Data..= target)
          ]
      )

instance Data.ToPath CreateHomeRegionControl where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHomeRegionControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHomeRegionControlResponse' smart constructor.
data CreateHomeRegionControlResponse = CreateHomeRegionControlResponse'
  { -- | This object is the @HomeRegionControl@ object that\'s returned by a
    -- successful call to @CreateHomeRegionControl@.
    homeRegionControl :: Prelude.Maybe HomeRegionControl,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHomeRegionControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeRegionControl', 'createHomeRegionControlResponse_homeRegionControl' - This object is the @HomeRegionControl@ object that\'s returned by a
-- successful call to @CreateHomeRegionControl@.
--
-- 'httpStatus', 'createHomeRegionControlResponse_httpStatus' - The response's http status code.
newCreateHomeRegionControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHomeRegionControlResponse
newCreateHomeRegionControlResponse pHttpStatus_ =
  CreateHomeRegionControlResponse'
    { homeRegionControl =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This object is the @HomeRegionControl@ object that\'s returned by a
-- successful call to @CreateHomeRegionControl@.
createHomeRegionControlResponse_homeRegionControl :: Lens.Lens' CreateHomeRegionControlResponse (Prelude.Maybe HomeRegionControl)
createHomeRegionControlResponse_homeRegionControl = Lens.lens (\CreateHomeRegionControlResponse' {homeRegionControl} -> homeRegionControl) (\s@CreateHomeRegionControlResponse' {} a -> s {homeRegionControl = a} :: CreateHomeRegionControlResponse)

-- | The response's http status code.
createHomeRegionControlResponse_httpStatus :: Lens.Lens' CreateHomeRegionControlResponse Prelude.Int
createHomeRegionControlResponse_httpStatus = Lens.lens (\CreateHomeRegionControlResponse' {httpStatus} -> httpStatus) (\s@CreateHomeRegionControlResponse' {} a -> s {httpStatus = a} :: CreateHomeRegionControlResponse)

instance
  Prelude.NFData
    CreateHomeRegionControlResponse
  where
  rnf CreateHomeRegionControlResponse' {..} =
    Prelude.rnf homeRegionControl
      `Prelude.seq` Prelude.rnf httpStatus
