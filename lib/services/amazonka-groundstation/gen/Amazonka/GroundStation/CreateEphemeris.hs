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
-- Module      : Amazonka.GroundStation.CreateEphemeris
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Ephemeris with the specified @EphemerisData@.
module Amazonka.GroundStation.CreateEphemeris
  ( -- * Creating a Request
    CreateEphemeris (..),
    newCreateEphemeris,

    -- * Request Lenses
    createEphemeris_tags,
    createEphemeris_expirationTime,
    createEphemeris_enabled,
    createEphemeris_kmsKeyArn,
    createEphemeris_priority,
    createEphemeris_ephemeris,
    createEphemeris_name,
    createEphemeris_satelliteId,

    -- * Destructuring the Response
    EphemerisIdResponse (..),
    newEphemerisIdResponse,

    -- * Response Lenses
    ephemerisIdResponse_ephemerisId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEphemeris' smart constructor.
data CreateEphemeris = CreateEphemeris'
  { -- | Tags assigned to an ephemeris.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An overall expiration time for the ephemeris in UTC, after which it will
    -- become @EXPIRED@.
    expirationTime :: Prelude.Maybe Data.POSIX,
    -- | Whether to set the ephemeris status to @ENABLED@ after validation.
    --
    -- Setting this to false will set the ephemeris status to @DISABLED@ after
    -- validation.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of a KMS key used to encrypt the ephemeris in Ground Station.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | Customer-provided priority score to establish the order in which
    -- overlapping ephemerides should be used.
    --
    -- The default for customer-provided ephemeris priority is 1, and higher
    -- numbers take precedence.
    --
    -- Priority must be 1 or greater
    priority :: Prelude.Maybe Prelude.Natural,
    -- | Ephemeris data.
    ephemeris :: Prelude.Maybe EphemerisData,
    -- | A name string associated with the ephemeris. Used as a human-readable
    -- identifier for the ephemeris.
    name :: Prelude.Text,
    -- | AWS Ground Station satellite ID for this ephemeris.
    satelliteId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEphemeris' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEphemeris_tags' - Tags assigned to an ephemeris.
--
-- 'expirationTime', 'createEphemeris_expirationTime' - An overall expiration time for the ephemeris in UTC, after which it will
-- become @EXPIRED@.
--
-- 'enabled', 'createEphemeris_enabled' - Whether to set the ephemeris status to @ENABLED@ after validation.
--
-- Setting this to false will set the ephemeris status to @DISABLED@ after
-- validation.
--
-- 'kmsKeyArn', 'createEphemeris_kmsKeyArn' - The ARN of a KMS key used to encrypt the ephemeris in Ground Station.
--
-- 'priority', 'createEphemeris_priority' - Customer-provided priority score to establish the order in which
-- overlapping ephemerides should be used.
--
-- The default for customer-provided ephemeris priority is 1, and higher
-- numbers take precedence.
--
-- Priority must be 1 or greater
--
-- 'ephemeris', 'createEphemeris_ephemeris' - Ephemeris data.
--
-- 'name', 'createEphemeris_name' - A name string associated with the ephemeris. Used as a human-readable
-- identifier for the ephemeris.
--
-- 'satelliteId', 'createEphemeris_satelliteId' - AWS Ground Station satellite ID for this ephemeris.
newCreateEphemeris ::
  -- | 'name'
  Prelude.Text ->
  -- | 'satelliteId'
  Prelude.Text ->
  CreateEphemeris
newCreateEphemeris pName_ pSatelliteId_ =
  CreateEphemeris'
    { tags = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      enabled = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      priority = Prelude.Nothing,
      ephemeris = Prelude.Nothing,
      name = pName_,
      satelliteId = pSatelliteId_
    }

-- | Tags assigned to an ephemeris.
createEphemeris_tags :: Lens.Lens' CreateEphemeris (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEphemeris_tags = Lens.lens (\CreateEphemeris' {tags} -> tags) (\s@CreateEphemeris' {} a -> s {tags = a} :: CreateEphemeris) Prelude.. Lens.mapping Lens.coerced

-- | An overall expiration time for the ephemeris in UTC, after which it will
-- become @EXPIRED@.
createEphemeris_expirationTime :: Lens.Lens' CreateEphemeris (Prelude.Maybe Prelude.UTCTime)
createEphemeris_expirationTime = Lens.lens (\CreateEphemeris' {expirationTime} -> expirationTime) (\s@CreateEphemeris' {} a -> s {expirationTime = a} :: CreateEphemeris) Prelude.. Lens.mapping Data._Time

-- | Whether to set the ephemeris status to @ENABLED@ after validation.
--
-- Setting this to false will set the ephemeris status to @DISABLED@ after
-- validation.
createEphemeris_enabled :: Lens.Lens' CreateEphemeris (Prelude.Maybe Prelude.Bool)
createEphemeris_enabled = Lens.lens (\CreateEphemeris' {enabled} -> enabled) (\s@CreateEphemeris' {} a -> s {enabled = a} :: CreateEphemeris)

-- | The ARN of a KMS key used to encrypt the ephemeris in Ground Station.
createEphemeris_kmsKeyArn :: Lens.Lens' CreateEphemeris (Prelude.Maybe Prelude.Text)
createEphemeris_kmsKeyArn = Lens.lens (\CreateEphemeris' {kmsKeyArn} -> kmsKeyArn) (\s@CreateEphemeris' {} a -> s {kmsKeyArn = a} :: CreateEphemeris)

-- | Customer-provided priority score to establish the order in which
-- overlapping ephemerides should be used.
--
-- The default for customer-provided ephemeris priority is 1, and higher
-- numbers take precedence.
--
-- Priority must be 1 or greater
createEphemeris_priority :: Lens.Lens' CreateEphemeris (Prelude.Maybe Prelude.Natural)
createEphemeris_priority = Lens.lens (\CreateEphemeris' {priority} -> priority) (\s@CreateEphemeris' {} a -> s {priority = a} :: CreateEphemeris)

-- | Ephemeris data.
createEphemeris_ephemeris :: Lens.Lens' CreateEphemeris (Prelude.Maybe EphemerisData)
createEphemeris_ephemeris = Lens.lens (\CreateEphemeris' {ephemeris} -> ephemeris) (\s@CreateEphemeris' {} a -> s {ephemeris = a} :: CreateEphemeris)

-- | A name string associated with the ephemeris. Used as a human-readable
-- identifier for the ephemeris.
createEphemeris_name :: Lens.Lens' CreateEphemeris Prelude.Text
createEphemeris_name = Lens.lens (\CreateEphemeris' {name} -> name) (\s@CreateEphemeris' {} a -> s {name = a} :: CreateEphemeris)

-- | AWS Ground Station satellite ID for this ephemeris.
createEphemeris_satelliteId :: Lens.Lens' CreateEphemeris Prelude.Text
createEphemeris_satelliteId = Lens.lens (\CreateEphemeris' {satelliteId} -> satelliteId) (\s@CreateEphemeris' {} a -> s {satelliteId = a} :: CreateEphemeris)

instance Core.AWSRequest CreateEphemeris where
  type
    AWSResponse CreateEphemeris =
      EphemerisIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateEphemeris where
  hashWithSalt _salt CreateEphemeris' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` ephemeris
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` satelliteId

instance Prelude.NFData CreateEphemeris where
  rnf CreateEphemeris' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf priority
      `Prelude.seq` Prelude.rnf ephemeris
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf satelliteId

instance Data.ToHeaders CreateEphemeris where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEphemeris where
  toJSON CreateEphemeris' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("expirationTime" Data..=)
              Prelude.<$> expirationTime,
            ("enabled" Data..=) Prelude.<$> enabled,
            ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("priority" Data..=) Prelude.<$> priority,
            ("ephemeris" Data..=) Prelude.<$> ephemeris,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("satelliteId" Data..= satelliteId)
          ]
      )

instance Data.ToPath CreateEphemeris where
  toPath = Prelude.const "/ephemeris"

instance Data.ToQuery CreateEphemeris where
  toQuery = Prelude.const Prelude.mempty
