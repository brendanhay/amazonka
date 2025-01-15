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
-- Module      : Amazonka.Glue.BatchGetBlueprints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a list of blueprints.
module Amazonka.Glue.BatchGetBlueprints
  ( -- * Creating a Request
    BatchGetBlueprints (..),
    newBatchGetBlueprints,

    -- * Request Lenses
    batchGetBlueprints_includeBlueprint,
    batchGetBlueprints_includeParameterSpec,
    batchGetBlueprints_names,

    -- * Destructuring the Response
    BatchGetBlueprintsResponse (..),
    newBatchGetBlueprintsResponse,

    -- * Response Lenses
    batchGetBlueprintsResponse_blueprints,
    batchGetBlueprintsResponse_missingBlueprints,
    batchGetBlueprintsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetBlueprints' smart constructor.
data BatchGetBlueprints = BatchGetBlueprints'
  { -- | Specifies whether or not to include the blueprint in the response.
    includeBlueprint :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether or not to include the parameters, as a JSON string,
    -- for the blueprint in the response.
    includeParameterSpec :: Prelude.Maybe Prelude.Bool,
    -- | A list of blueprint names.
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetBlueprints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeBlueprint', 'batchGetBlueprints_includeBlueprint' - Specifies whether or not to include the blueprint in the response.
--
-- 'includeParameterSpec', 'batchGetBlueprints_includeParameterSpec' - Specifies whether or not to include the parameters, as a JSON string,
-- for the blueprint in the response.
--
-- 'names', 'batchGetBlueprints_names' - A list of blueprint names.
newBatchGetBlueprints ::
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetBlueprints
newBatchGetBlueprints pNames_ =
  BatchGetBlueprints'
    { includeBlueprint =
        Prelude.Nothing,
      includeParameterSpec = Prelude.Nothing,
      names = Lens.coerced Lens.# pNames_
    }

-- | Specifies whether or not to include the blueprint in the response.
batchGetBlueprints_includeBlueprint :: Lens.Lens' BatchGetBlueprints (Prelude.Maybe Prelude.Bool)
batchGetBlueprints_includeBlueprint = Lens.lens (\BatchGetBlueprints' {includeBlueprint} -> includeBlueprint) (\s@BatchGetBlueprints' {} a -> s {includeBlueprint = a} :: BatchGetBlueprints)

-- | Specifies whether or not to include the parameters, as a JSON string,
-- for the blueprint in the response.
batchGetBlueprints_includeParameterSpec :: Lens.Lens' BatchGetBlueprints (Prelude.Maybe Prelude.Bool)
batchGetBlueprints_includeParameterSpec = Lens.lens (\BatchGetBlueprints' {includeParameterSpec} -> includeParameterSpec) (\s@BatchGetBlueprints' {} a -> s {includeParameterSpec = a} :: BatchGetBlueprints)

-- | A list of blueprint names.
batchGetBlueprints_names :: Lens.Lens' BatchGetBlueprints (Prelude.NonEmpty Prelude.Text)
batchGetBlueprints_names = Lens.lens (\BatchGetBlueprints' {names} -> names) (\s@BatchGetBlueprints' {} a -> s {names = a} :: BatchGetBlueprints) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetBlueprints where
  type
    AWSResponse BatchGetBlueprints =
      BatchGetBlueprintsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetBlueprintsResponse'
            Prelude.<$> (x Data..?> "Blueprints" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "MissingBlueprints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetBlueprints where
  hashWithSalt _salt BatchGetBlueprints' {..} =
    _salt
      `Prelude.hashWithSalt` includeBlueprint
      `Prelude.hashWithSalt` includeParameterSpec
      `Prelude.hashWithSalt` names

instance Prelude.NFData BatchGetBlueprints where
  rnf BatchGetBlueprints' {..} =
    Prelude.rnf includeBlueprint `Prelude.seq`
      Prelude.rnf includeParameterSpec `Prelude.seq`
        Prelude.rnf names

instance Data.ToHeaders BatchGetBlueprints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.BatchGetBlueprints" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetBlueprints where
  toJSON BatchGetBlueprints' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludeBlueprint" Data..=)
              Prelude.<$> includeBlueprint,
            ("IncludeParameterSpec" Data..=)
              Prelude.<$> includeParameterSpec,
            Prelude.Just ("Names" Data..= names)
          ]
      )

instance Data.ToPath BatchGetBlueprints where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetBlueprints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetBlueprintsResponse' smart constructor.
data BatchGetBlueprintsResponse = BatchGetBlueprintsResponse'
  { -- | Returns a list of blueprint as a @Blueprints@ object.
    blueprints :: Prelude.Maybe [Blueprint],
    -- | Returns a list of @BlueprintNames@ that were not found.
    missingBlueprints :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetBlueprintsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprints', 'batchGetBlueprintsResponse_blueprints' - Returns a list of blueprint as a @Blueprints@ object.
--
-- 'missingBlueprints', 'batchGetBlueprintsResponse_missingBlueprints' - Returns a list of @BlueprintNames@ that were not found.
--
-- 'httpStatus', 'batchGetBlueprintsResponse_httpStatus' - The response's http status code.
newBatchGetBlueprintsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetBlueprintsResponse
newBatchGetBlueprintsResponse pHttpStatus_ =
  BatchGetBlueprintsResponse'
    { blueprints =
        Prelude.Nothing,
      missingBlueprints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of blueprint as a @Blueprints@ object.
batchGetBlueprintsResponse_blueprints :: Lens.Lens' BatchGetBlueprintsResponse (Prelude.Maybe [Blueprint])
batchGetBlueprintsResponse_blueprints = Lens.lens (\BatchGetBlueprintsResponse' {blueprints} -> blueprints) (\s@BatchGetBlueprintsResponse' {} a -> s {blueprints = a} :: BatchGetBlueprintsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns a list of @BlueprintNames@ that were not found.
batchGetBlueprintsResponse_missingBlueprints :: Lens.Lens' BatchGetBlueprintsResponse (Prelude.Maybe [Prelude.Text])
batchGetBlueprintsResponse_missingBlueprints = Lens.lens (\BatchGetBlueprintsResponse' {missingBlueprints} -> missingBlueprints) (\s@BatchGetBlueprintsResponse' {} a -> s {missingBlueprints = a} :: BatchGetBlueprintsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetBlueprintsResponse_httpStatus :: Lens.Lens' BatchGetBlueprintsResponse Prelude.Int
batchGetBlueprintsResponse_httpStatus = Lens.lens (\BatchGetBlueprintsResponse' {httpStatus} -> httpStatus) (\s@BatchGetBlueprintsResponse' {} a -> s {httpStatus = a} :: BatchGetBlueprintsResponse)

instance Prelude.NFData BatchGetBlueprintsResponse where
  rnf BatchGetBlueprintsResponse' {..} =
    Prelude.rnf blueprints `Prelude.seq`
      Prelude.rnf missingBlueprints `Prelude.seq`
        Prelude.rnf httpStatus
