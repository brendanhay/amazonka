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
-- Module      : Amazonka.Athena.CreateCapacityReservation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a capacity reservation with the specified name and number of
-- requested data processing units.
module Amazonka.Athena.CreateCapacityReservation
  ( -- * Creating a Request
    CreateCapacityReservation (..),
    newCreateCapacityReservation,

    -- * Request Lenses
    createCapacityReservation_tags,
    createCapacityReservation_targetDpus,
    createCapacityReservation_name,

    -- * Destructuring the Response
    CreateCapacityReservationResponse (..),
    newCreateCapacityReservationResponse,

    -- * Response Lenses
    createCapacityReservationResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCapacityReservation' smart constructor.
data CreateCapacityReservation = CreateCapacityReservation'
  { -- | The tags for the capacity reservation.
    tags :: Prelude.Maybe [Tag],
    -- | The number of requested data processing units.
    targetDpus :: Prelude.Natural,
    -- | The name of the capacity reservation to create.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCapacityReservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCapacityReservation_tags' - The tags for the capacity reservation.
--
-- 'targetDpus', 'createCapacityReservation_targetDpus' - The number of requested data processing units.
--
-- 'name', 'createCapacityReservation_name' - The name of the capacity reservation to create.
newCreateCapacityReservation ::
  -- | 'targetDpus'
  Prelude.Natural ->
  -- | 'name'
  Prelude.Text ->
  CreateCapacityReservation
newCreateCapacityReservation pTargetDpus_ pName_ =
  CreateCapacityReservation'
    { tags = Prelude.Nothing,
      targetDpus = pTargetDpus_,
      name = pName_
    }

-- | The tags for the capacity reservation.
createCapacityReservation_tags :: Lens.Lens' CreateCapacityReservation (Prelude.Maybe [Tag])
createCapacityReservation_tags = Lens.lens (\CreateCapacityReservation' {tags} -> tags) (\s@CreateCapacityReservation' {} a -> s {tags = a} :: CreateCapacityReservation) Prelude.. Lens.mapping Lens.coerced

-- | The number of requested data processing units.
createCapacityReservation_targetDpus :: Lens.Lens' CreateCapacityReservation Prelude.Natural
createCapacityReservation_targetDpus = Lens.lens (\CreateCapacityReservation' {targetDpus} -> targetDpus) (\s@CreateCapacityReservation' {} a -> s {targetDpus = a} :: CreateCapacityReservation)

-- | The name of the capacity reservation to create.
createCapacityReservation_name :: Lens.Lens' CreateCapacityReservation Prelude.Text
createCapacityReservation_name = Lens.lens (\CreateCapacityReservation' {name} -> name) (\s@CreateCapacityReservation' {} a -> s {name = a} :: CreateCapacityReservation)

instance Core.AWSRequest CreateCapacityReservation where
  type
    AWSResponse CreateCapacityReservation =
      CreateCapacityReservationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateCapacityReservationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCapacityReservation where
  hashWithSalt _salt CreateCapacityReservation' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetDpus
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateCapacityReservation where
  rnf CreateCapacityReservation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetDpus
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateCapacityReservation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.CreateCapacityReservation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCapacityReservation where
  toJSON CreateCapacityReservation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("TargetDpus" Data..= targetDpus),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateCapacityReservation where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCapacityReservation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCapacityReservationResponse' smart constructor.
data CreateCapacityReservationResponse = CreateCapacityReservationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCapacityReservationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCapacityReservationResponse_httpStatus' - The response's http status code.
newCreateCapacityReservationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCapacityReservationResponse
newCreateCapacityReservationResponse pHttpStatus_ =
  CreateCapacityReservationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createCapacityReservationResponse_httpStatus :: Lens.Lens' CreateCapacityReservationResponse Prelude.Int
createCapacityReservationResponse_httpStatus = Lens.lens (\CreateCapacityReservationResponse' {httpStatus} -> httpStatus) (\s@CreateCapacityReservationResponse' {} a -> s {httpStatus = a} :: CreateCapacityReservationResponse)

instance
  Prelude.NFData
    CreateCapacityReservationResponse
  where
  rnf CreateCapacityReservationResponse' {..} =
    Prelude.rnf httpStatus
