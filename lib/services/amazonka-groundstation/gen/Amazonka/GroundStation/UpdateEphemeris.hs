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
-- Module      : Amazonka.GroundStation.UpdateEphemeris
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing ephemeris
module Amazonka.GroundStation.UpdateEphemeris
  ( -- * Creating a Request
    UpdateEphemeris (..),
    newUpdateEphemeris,

    -- * Request Lenses
    updateEphemeris_name,
    updateEphemeris_priority,
    updateEphemeris_enabled,
    updateEphemeris_ephemerisId,

    -- * Destructuring the Response
    EphemerisIdResponse (..),
    newEphemerisIdResponse,

    -- * Response Lenses
    ephemerisIdResponse_ephemerisId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEphemeris' smart constructor.
data UpdateEphemeris = UpdateEphemeris'
  { -- | A name string associated with the ephemeris. Used as a human-readable
    -- identifier for the ephemeris.
    name :: Prelude.Maybe Prelude.Text,
    -- | Customer-provided priority score to establish the order in which
    -- overlapping ephemerides should be used.
    --
    -- The default for customer-provided ephemeris priority is 1, and higher
    -- numbers take precedence.
    --
    -- Priority must be 1 or greater
    priority :: Prelude.Maybe Prelude.Natural,
    -- | Whether the ephemeris is enabled or not. Changing this value will not
    -- require the ephemeris to be re-validated.
    enabled :: Prelude.Bool,
    -- | The AWS Ground Station ephemeris ID.
    ephemerisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEphemeris' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateEphemeris_name' - A name string associated with the ephemeris. Used as a human-readable
-- identifier for the ephemeris.
--
-- 'priority', 'updateEphemeris_priority' - Customer-provided priority score to establish the order in which
-- overlapping ephemerides should be used.
--
-- The default for customer-provided ephemeris priority is 1, and higher
-- numbers take precedence.
--
-- Priority must be 1 or greater
--
-- 'enabled', 'updateEphemeris_enabled' - Whether the ephemeris is enabled or not. Changing this value will not
-- require the ephemeris to be re-validated.
--
-- 'ephemerisId', 'updateEphemeris_ephemerisId' - The AWS Ground Station ephemeris ID.
newUpdateEphemeris ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'ephemerisId'
  Prelude.Text ->
  UpdateEphemeris
newUpdateEphemeris pEnabled_ pEphemerisId_ =
  UpdateEphemeris'
    { name = Prelude.Nothing,
      priority = Prelude.Nothing,
      enabled = pEnabled_,
      ephemerisId = pEphemerisId_
    }

-- | A name string associated with the ephemeris. Used as a human-readable
-- identifier for the ephemeris.
updateEphemeris_name :: Lens.Lens' UpdateEphemeris (Prelude.Maybe Prelude.Text)
updateEphemeris_name = Lens.lens (\UpdateEphemeris' {name} -> name) (\s@UpdateEphemeris' {} a -> s {name = a} :: UpdateEphemeris)

-- | Customer-provided priority score to establish the order in which
-- overlapping ephemerides should be used.
--
-- The default for customer-provided ephemeris priority is 1, and higher
-- numbers take precedence.
--
-- Priority must be 1 or greater
updateEphemeris_priority :: Lens.Lens' UpdateEphemeris (Prelude.Maybe Prelude.Natural)
updateEphemeris_priority = Lens.lens (\UpdateEphemeris' {priority} -> priority) (\s@UpdateEphemeris' {} a -> s {priority = a} :: UpdateEphemeris)

-- | Whether the ephemeris is enabled or not. Changing this value will not
-- require the ephemeris to be re-validated.
updateEphemeris_enabled :: Lens.Lens' UpdateEphemeris Prelude.Bool
updateEphemeris_enabled = Lens.lens (\UpdateEphemeris' {enabled} -> enabled) (\s@UpdateEphemeris' {} a -> s {enabled = a} :: UpdateEphemeris)

-- | The AWS Ground Station ephemeris ID.
updateEphemeris_ephemerisId :: Lens.Lens' UpdateEphemeris Prelude.Text
updateEphemeris_ephemerisId = Lens.lens (\UpdateEphemeris' {ephemerisId} -> ephemerisId) (\s@UpdateEphemeris' {} a -> s {ephemerisId = a} :: UpdateEphemeris)

instance Core.AWSRequest UpdateEphemeris where
  type
    AWSResponse UpdateEphemeris =
      EphemerisIdResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateEphemeris where
  hashWithSalt _salt UpdateEphemeris' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` ephemerisId

instance Prelude.NFData UpdateEphemeris where
  rnf UpdateEphemeris' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf ephemerisId

instance Core.ToHeaders UpdateEphemeris where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateEphemeris where
  toJSON UpdateEphemeris' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("priority" Core..=) Prelude.<$> priority,
            Prelude.Just ("enabled" Core..= enabled)
          ]
      )

instance Core.ToPath UpdateEphemeris where
  toPath UpdateEphemeris' {..} =
    Prelude.mconcat
      ["/ephemeris/", Core.toBS ephemerisId]

instance Core.ToQuery UpdateEphemeris where
  toQuery = Prelude.const Prelude.mempty
