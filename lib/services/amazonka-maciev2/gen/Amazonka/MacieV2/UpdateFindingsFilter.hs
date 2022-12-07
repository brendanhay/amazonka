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
-- Module      : Amazonka.MacieV2.UpdateFindingsFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the criteria and other settings for a findings filter.
module Amazonka.MacieV2.UpdateFindingsFilter
  ( -- * Creating a Request
    UpdateFindingsFilter (..),
    newUpdateFindingsFilter,

    -- * Request Lenses
    updateFindingsFilter_name,
    updateFindingsFilter_clientToken,
    updateFindingsFilter_findingCriteria,
    updateFindingsFilter_description,
    updateFindingsFilter_action,
    updateFindingsFilter_position,
    updateFindingsFilter_id,

    -- * Destructuring the Response
    UpdateFindingsFilterResponse (..),
    newUpdateFindingsFilterResponse,

    -- * Response Lenses
    updateFindingsFilterResponse_arn,
    updateFindingsFilterResponse_id,
    updateFindingsFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFindingsFilter' smart constructor.
data UpdateFindingsFilter = UpdateFindingsFilter'
  { -- | A custom name for the filter. The name must contain at least 3
    -- characters and can contain as many as 64 characters.
    --
    -- We strongly recommend that you avoid including any sensitive data in the
    -- name of a filter. Other users might be able to see this name, depending
    -- on the actions that they\'re allowed to perform in Amazon Macie.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The criteria to use to filter findings.
    findingCriteria :: Prelude.Maybe FindingCriteria,
    -- | A custom description of the filter. The description can contain as many
    -- as 512 characters.
    --
    -- We strongly recommend that you avoid including any sensitive data in the
    -- description of a filter. Other users might be able to see this
    -- description, depending on the actions that they\'re allowed to perform
    -- in Amazon Macie.
    description :: Prelude.Maybe Prelude.Text,
    -- | The action to perform on findings that meet the filter criteria
    -- (findingCriteria). Valid values are: ARCHIVE, suppress (automatically
    -- archive) the findings; and, NOOP, don\'t perform any action on the
    -- findings.
    action :: Prelude.Maybe FindingsFilterAction,
    -- | The position of the filter in the list of saved filters on the Amazon
    -- Macie console. This value also determines the order in which the filter
    -- is applied to findings, relative to other filters that are also applied
    -- to the findings.
    position :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFindingsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateFindingsFilter_name' - A custom name for the filter. The name must contain at least 3
-- characters and can contain as many as 64 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- name of a filter. Other users might be able to see this name, depending
-- on the actions that they\'re allowed to perform in Amazon Macie.
--
-- 'clientToken', 'updateFindingsFilter_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'findingCriteria', 'updateFindingsFilter_findingCriteria' - The criteria to use to filter findings.
--
-- 'description', 'updateFindingsFilter_description' - A custom description of the filter. The description can contain as many
-- as 512 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- description of a filter. Other users might be able to see this
-- description, depending on the actions that they\'re allowed to perform
-- in Amazon Macie.
--
-- 'action', 'updateFindingsFilter_action' - The action to perform on findings that meet the filter criteria
-- (findingCriteria). Valid values are: ARCHIVE, suppress (automatically
-- archive) the findings; and, NOOP, don\'t perform any action on the
-- findings.
--
-- 'position', 'updateFindingsFilter_position' - The position of the filter in the list of saved filters on the Amazon
-- Macie console. This value also determines the order in which the filter
-- is applied to findings, relative to other filters that are also applied
-- to the findings.
--
-- 'id', 'updateFindingsFilter_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newUpdateFindingsFilter ::
  -- | 'id'
  Prelude.Text ->
  UpdateFindingsFilter
newUpdateFindingsFilter pId_ =
  UpdateFindingsFilter'
    { name = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      findingCriteria = Prelude.Nothing,
      description = Prelude.Nothing,
      action = Prelude.Nothing,
      position = Prelude.Nothing,
      id = pId_
    }

-- | A custom name for the filter. The name must contain at least 3
-- characters and can contain as many as 64 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- name of a filter. Other users might be able to see this name, depending
-- on the actions that they\'re allowed to perform in Amazon Macie.
updateFindingsFilter_name :: Lens.Lens' UpdateFindingsFilter (Prelude.Maybe Prelude.Text)
updateFindingsFilter_name = Lens.lens (\UpdateFindingsFilter' {name} -> name) (\s@UpdateFindingsFilter' {} a -> s {name = a} :: UpdateFindingsFilter)

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
updateFindingsFilter_clientToken :: Lens.Lens' UpdateFindingsFilter (Prelude.Maybe Prelude.Text)
updateFindingsFilter_clientToken = Lens.lens (\UpdateFindingsFilter' {clientToken} -> clientToken) (\s@UpdateFindingsFilter' {} a -> s {clientToken = a} :: UpdateFindingsFilter)

-- | The criteria to use to filter findings.
updateFindingsFilter_findingCriteria :: Lens.Lens' UpdateFindingsFilter (Prelude.Maybe FindingCriteria)
updateFindingsFilter_findingCriteria = Lens.lens (\UpdateFindingsFilter' {findingCriteria} -> findingCriteria) (\s@UpdateFindingsFilter' {} a -> s {findingCriteria = a} :: UpdateFindingsFilter)

-- | A custom description of the filter. The description can contain as many
-- as 512 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- description of a filter. Other users might be able to see this
-- description, depending on the actions that they\'re allowed to perform
-- in Amazon Macie.
updateFindingsFilter_description :: Lens.Lens' UpdateFindingsFilter (Prelude.Maybe Prelude.Text)
updateFindingsFilter_description = Lens.lens (\UpdateFindingsFilter' {description} -> description) (\s@UpdateFindingsFilter' {} a -> s {description = a} :: UpdateFindingsFilter)

-- | The action to perform on findings that meet the filter criteria
-- (findingCriteria). Valid values are: ARCHIVE, suppress (automatically
-- archive) the findings; and, NOOP, don\'t perform any action on the
-- findings.
updateFindingsFilter_action :: Lens.Lens' UpdateFindingsFilter (Prelude.Maybe FindingsFilterAction)
updateFindingsFilter_action = Lens.lens (\UpdateFindingsFilter' {action} -> action) (\s@UpdateFindingsFilter' {} a -> s {action = a} :: UpdateFindingsFilter)

-- | The position of the filter in the list of saved filters on the Amazon
-- Macie console. This value also determines the order in which the filter
-- is applied to findings, relative to other filters that are also applied
-- to the findings.
updateFindingsFilter_position :: Lens.Lens' UpdateFindingsFilter (Prelude.Maybe Prelude.Int)
updateFindingsFilter_position = Lens.lens (\UpdateFindingsFilter' {position} -> position) (\s@UpdateFindingsFilter' {} a -> s {position = a} :: UpdateFindingsFilter)

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
updateFindingsFilter_id :: Lens.Lens' UpdateFindingsFilter Prelude.Text
updateFindingsFilter_id = Lens.lens (\UpdateFindingsFilter' {id} -> id) (\s@UpdateFindingsFilter' {} a -> s {id = a} :: UpdateFindingsFilter)

instance Core.AWSRequest UpdateFindingsFilter where
  type
    AWSResponse UpdateFindingsFilter =
      UpdateFindingsFilterResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateFindingsFilterResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFindingsFilter where
  hashWithSalt _salt UpdateFindingsFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` findingCriteria
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateFindingsFilter where
  rnf UpdateFindingsFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf findingCriteria
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateFindingsFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFindingsFilter where
  toJSON UpdateFindingsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("findingCriteria" Data..=)
              Prelude.<$> findingCriteria,
            ("description" Data..=) Prelude.<$> description,
            ("action" Data..=) Prelude.<$> action,
            ("position" Data..=) Prelude.<$> position
          ]
      )

instance Data.ToPath UpdateFindingsFilter where
  toPath UpdateFindingsFilter' {..} =
    Prelude.mconcat ["/findingsfilters/", Data.toBS id]

instance Data.ToQuery UpdateFindingsFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFindingsFilterResponse' smart constructor.
data UpdateFindingsFilterResponse = UpdateFindingsFilterResponse'
  { -- | The Amazon Resource Name (ARN) of the filter that was updated.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the filter that was updated.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFindingsFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateFindingsFilterResponse_arn' - The Amazon Resource Name (ARN) of the filter that was updated.
--
-- 'id', 'updateFindingsFilterResponse_id' - The unique identifier for the filter that was updated.
--
-- 'httpStatus', 'updateFindingsFilterResponse_httpStatus' - The response's http status code.
newUpdateFindingsFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFindingsFilterResponse
newUpdateFindingsFilterResponse pHttpStatus_ =
  UpdateFindingsFilterResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the filter that was updated.
updateFindingsFilterResponse_arn :: Lens.Lens' UpdateFindingsFilterResponse (Prelude.Maybe Prelude.Text)
updateFindingsFilterResponse_arn = Lens.lens (\UpdateFindingsFilterResponse' {arn} -> arn) (\s@UpdateFindingsFilterResponse' {} a -> s {arn = a} :: UpdateFindingsFilterResponse)

-- | The unique identifier for the filter that was updated.
updateFindingsFilterResponse_id :: Lens.Lens' UpdateFindingsFilterResponse (Prelude.Maybe Prelude.Text)
updateFindingsFilterResponse_id = Lens.lens (\UpdateFindingsFilterResponse' {id} -> id) (\s@UpdateFindingsFilterResponse' {} a -> s {id = a} :: UpdateFindingsFilterResponse)

-- | The response's http status code.
updateFindingsFilterResponse_httpStatus :: Lens.Lens' UpdateFindingsFilterResponse Prelude.Int
updateFindingsFilterResponse_httpStatus = Lens.lens (\UpdateFindingsFilterResponse' {httpStatus} -> httpStatus) (\s@UpdateFindingsFilterResponse' {} a -> s {httpStatus = a} :: UpdateFindingsFilterResponse)

instance Prelude.NFData UpdateFindingsFilterResponse where
  rnf UpdateFindingsFilterResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
