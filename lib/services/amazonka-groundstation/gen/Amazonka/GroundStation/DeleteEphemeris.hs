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
-- Module      : Amazonka.GroundStation.DeleteEphemeris
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an ephemeris
module Amazonka.GroundStation.DeleteEphemeris
  ( -- * Creating a Request
    DeleteEphemeris (..),
    newDeleteEphemeris,

    -- * Request Lenses
    deleteEphemeris_ephemerisId,

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

-- | /See:/ 'newDeleteEphemeris' smart constructor.
data DeleteEphemeris = DeleteEphemeris'
  { -- | The AWS Ground Station ephemeris ID.
    ephemerisId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEphemeris' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ephemerisId', 'deleteEphemeris_ephemerisId' - The AWS Ground Station ephemeris ID.
newDeleteEphemeris ::
  -- | 'ephemerisId'
  Prelude.Text ->
  DeleteEphemeris
newDeleteEphemeris pEphemerisId_ =
  DeleteEphemeris' {ephemerisId = pEphemerisId_}

-- | The AWS Ground Station ephemeris ID.
deleteEphemeris_ephemerisId :: Lens.Lens' DeleteEphemeris Prelude.Text
deleteEphemeris_ephemerisId = Lens.lens (\DeleteEphemeris' {ephemerisId} -> ephemerisId) (\s@DeleteEphemeris' {} a -> s {ephemerisId = a} :: DeleteEphemeris)

instance Core.AWSRequest DeleteEphemeris where
  type
    AWSResponse DeleteEphemeris =
      EphemerisIdResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DeleteEphemeris where
  hashWithSalt _salt DeleteEphemeris' {..} =
    _salt `Prelude.hashWithSalt` ephemerisId

instance Prelude.NFData DeleteEphemeris where
  rnf DeleteEphemeris' {..} = Prelude.rnf ephemerisId

instance Data.ToHeaders DeleteEphemeris where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEphemeris where
  toPath DeleteEphemeris' {..} =
    Prelude.mconcat
      ["/ephemeris/", Data.toBS ephemerisId]

instance Data.ToQuery DeleteEphemeris where
  toQuery = Prelude.const Prelude.mempty
