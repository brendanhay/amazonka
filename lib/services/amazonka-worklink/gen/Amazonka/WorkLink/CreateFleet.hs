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
-- Module      : Amazonka.WorkLink.CreateFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet. A fleet consists of resources and the configuration
-- that delivers associated websites to authorized users who download and
-- set up the Amazon WorkLink app.
module Amazonka.WorkLink.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_tags,
    createFleet_displayName,
    createFleet_optimizeForEndUserLocation,
    createFleet_fleetName,

    -- * Destructuring the Response
    CreateFleetResponse (..),
    newCreateFleetResponse,

    -- * Response Lenses
    createFleetResponse_fleetArn,
    createFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | The tags to add to the resource. A tag is a key-value pair.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The fleet name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The option to optimize for better performance by routing traffic through
    -- the closest AWS Region to users, which may be outside of your home
    -- Region.
    optimizeForEndUserLocation :: Prelude.Maybe Prelude.Bool,
    -- | A unique name for the fleet.
    fleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFleet_tags' - The tags to add to the resource. A tag is a key-value pair.
--
-- 'displayName', 'createFleet_displayName' - The fleet name to display.
--
-- 'optimizeForEndUserLocation', 'createFleet_optimizeForEndUserLocation' - The option to optimize for better performance by routing traffic through
-- the closest AWS Region to users, which may be outside of your home
-- Region.
--
-- 'fleetName', 'createFleet_fleetName' - A unique name for the fleet.
newCreateFleet ::
  -- | 'fleetName'
  Prelude.Text ->
  CreateFleet
newCreateFleet pFleetName_ =
  CreateFleet'
    { tags = Prelude.Nothing,
      displayName = Prelude.Nothing,
      optimizeForEndUserLocation = Prelude.Nothing,
      fleetName = pFleetName_
    }

-- | The tags to add to the resource. A tag is a key-value pair.
createFleet_tags :: Lens.Lens' CreateFleet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFleet_tags = Lens.lens (\CreateFleet' {tags} -> tags) (\s@CreateFleet' {} a -> s {tags = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The fleet name to display.
createFleet_displayName :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Text)
createFleet_displayName = Lens.lens (\CreateFleet' {displayName} -> displayName) (\s@CreateFleet' {} a -> s {displayName = a} :: CreateFleet)

-- | The option to optimize for better performance by routing traffic through
-- the closest AWS Region to users, which may be outside of your home
-- Region.
createFleet_optimizeForEndUserLocation :: Lens.Lens' CreateFleet (Prelude.Maybe Prelude.Bool)
createFleet_optimizeForEndUserLocation = Lens.lens (\CreateFleet' {optimizeForEndUserLocation} -> optimizeForEndUserLocation) (\s@CreateFleet' {} a -> s {optimizeForEndUserLocation = a} :: CreateFleet)

-- | A unique name for the fleet.
createFleet_fleetName :: Lens.Lens' CreateFleet Prelude.Text
createFleet_fleetName = Lens.lens (\CreateFleet' {fleetName} -> fleetName) (\s@CreateFleet' {} a -> s {fleetName = a} :: CreateFleet)

instance Core.AWSRequest CreateFleet where
  type AWSResponse CreateFleet = CreateFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Prelude.<$> (x Core..?> "FleetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleet where
  hashWithSalt _salt CreateFleet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` optimizeForEndUserLocation
      `Prelude.hashWithSalt` fleetName

instance Prelude.NFData CreateFleet where
  rnf CreateFleet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf optimizeForEndUserLocation
      `Prelude.seq` Prelude.rnf fleetName

instance Core.ToHeaders CreateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("DisplayName" Core..=) Prelude.<$> displayName,
            ("OptimizeForEndUserLocation" Core..=)
              Prelude.<$> optimizeForEndUserLocation,
            Prelude.Just ("FleetName" Core..= fleetName)
          ]
      )

instance Core.ToPath CreateFleet where
  toPath = Prelude.const "/createFleet"

instance Core.ToQuery CreateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | The Amazon Resource Name (ARN) of the fleet.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetArn', 'createFleetResponse_fleetArn' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'httpStatus', 'createFleetResponse_httpStatus' - The response's http status code.
newCreateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetResponse
newCreateFleetResponse pHttpStatus_ =
  CreateFleetResponse'
    { fleetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the fleet.
createFleetResponse_fleetArn :: Lens.Lens' CreateFleetResponse (Prelude.Maybe Prelude.Text)
createFleetResponse_fleetArn = Lens.lens (\CreateFleetResponse' {fleetArn} -> fleetArn) (\s@CreateFleetResponse' {} a -> s {fleetArn = a} :: CreateFleetResponse)

-- | The response's http status code.
createFleetResponse_httpStatus :: Lens.Lens' CreateFleetResponse Prelude.Int
createFleetResponse_httpStatus = Lens.lens (\CreateFleetResponse' {httpStatus} -> httpStatus) (\s@CreateFleetResponse' {} a -> s {httpStatus = a} :: CreateFleetResponse)

instance Prelude.NFData CreateFleetResponse where
  rnf CreateFleetResponse' {..} =
    Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf httpStatus
