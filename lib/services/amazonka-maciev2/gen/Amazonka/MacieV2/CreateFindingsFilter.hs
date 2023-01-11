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
-- Module      : Amazonka.MacieV2.CreateFindingsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and defines the criteria and other settings for a findings
-- filter.
module Amazonka.MacieV2.CreateFindingsFilter
  ( -- * Creating a Request
    CreateFindingsFilter (..),
    newCreateFindingsFilter,

    -- * Request Lenses
    createFindingsFilter_clientToken,
    createFindingsFilter_description,
    createFindingsFilter_position,
    createFindingsFilter_tags,
    createFindingsFilter_action,
    createFindingsFilter_findingCriteria,
    createFindingsFilter_name,

    -- * Destructuring the Response
    CreateFindingsFilterResponse (..),
    newCreateFindingsFilterResponse,

    -- * Response Lenses
    createFindingsFilterResponse_arn,
    createFindingsFilterResponse_id,
    createFindingsFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFindingsFilter' smart constructor.
data CreateFindingsFilter = CreateFindingsFilter'
  { -- | A unique, case-sensitive token that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A custom description of the filter. The description can contain as many
    -- as 512 characters.
    --
    -- We strongly recommend that you avoid including any sensitive data in the
    -- description of a filter. Other users of your account might be able to
    -- see this description, depending on the actions that they\'re allowed to
    -- perform in Amazon Macie.
    description :: Prelude.Maybe Prelude.Text,
    -- | The position of the filter in the list of saved filters on the Amazon
    -- Macie console. This value also determines the order in which the filter
    -- is applied to findings, relative to other filters that are also applied
    -- to the findings.
    position :: Prelude.Maybe Prelude.Int,
    -- | A map of key-value pairs that specifies the tags to associate with the
    -- filter.
    --
    -- A findings filter can have a maximum of 50 tags. Each tag consists of a
    -- tag key and an associated tag value. The maximum length of a tag key is
    -- 128 characters. The maximum length of a tag value is 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The action to perform on findings that match the filter criteria
    -- (findingCriteria). Valid values are: ARCHIVE, suppress (automatically
    -- archive) the findings; and, NOOP, don\'t perform any action on the
    -- findings.
    action :: FindingsFilterAction,
    -- | The criteria to use to filter findings.
    findingCriteria :: FindingCriteria,
    -- | A custom name for the filter. The name must contain at least 3
    -- characters and can contain as many as 64 characters.
    --
    -- We strongly recommend that you avoid including any sensitive data in the
    -- name of a filter. Other users of your account might be able to see this
    -- name, depending on the actions that they\'re allowed to perform in
    -- Amazon Macie.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFindingsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createFindingsFilter_clientToken' - A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createFindingsFilter_description' - A custom description of the filter. The description can contain as many
-- as 512 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- description of a filter. Other users of your account might be able to
-- see this description, depending on the actions that they\'re allowed to
-- perform in Amazon Macie.
--
-- 'position', 'createFindingsFilter_position' - The position of the filter in the list of saved filters on the Amazon
-- Macie console. This value also determines the order in which the filter
-- is applied to findings, relative to other filters that are also applied
-- to the findings.
--
-- 'tags', 'createFindingsFilter_tags' - A map of key-value pairs that specifies the tags to associate with the
-- filter.
--
-- A findings filter can have a maximum of 50 tags. Each tag consists of a
-- tag key and an associated tag value. The maximum length of a tag key is
-- 128 characters. The maximum length of a tag value is 256 characters.
--
-- 'action', 'createFindingsFilter_action' - The action to perform on findings that match the filter criteria
-- (findingCriteria). Valid values are: ARCHIVE, suppress (automatically
-- archive) the findings; and, NOOP, don\'t perform any action on the
-- findings.
--
-- 'findingCriteria', 'createFindingsFilter_findingCriteria' - The criteria to use to filter findings.
--
-- 'name', 'createFindingsFilter_name' - A custom name for the filter. The name must contain at least 3
-- characters and can contain as many as 64 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- name of a filter. Other users of your account might be able to see this
-- name, depending on the actions that they\'re allowed to perform in
-- Amazon Macie.
newCreateFindingsFilter ::
  -- | 'action'
  FindingsFilterAction ->
  -- | 'findingCriteria'
  FindingCriteria ->
  -- | 'name'
  Prelude.Text ->
  CreateFindingsFilter
newCreateFindingsFilter
  pAction_
  pFindingCriteria_
  pName_ =
    CreateFindingsFilter'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        position = Prelude.Nothing,
        tags = Prelude.Nothing,
        action = pAction_,
        findingCriteria = pFindingCriteria_,
        name = pName_
      }

-- | A unique, case-sensitive token that you provide to ensure the
-- idempotency of the request.
createFindingsFilter_clientToken :: Lens.Lens' CreateFindingsFilter (Prelude.Maybe Prelude.Text)
createFindingsFilter_clientToken = Lens.lens (\CreateFindingsFilter' {clientToken} -> clientToken) (\s@CreateFindingsFilter' {} a -> s {clientToken = a} :: CreateFindingsFilter)

-- | A custom description of the filter. The description can contain as many
-- as 512 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- description of a filter. Other users of your account might be able to
-- see this description, depending on the actions that they\'re allowed to
-- perform in Amazon Macie.
createFindingsFilter_description :: Lens.Lens' CreateFindingsFilter (Prelude.Maybe Prelude.Text)
createFindingsFilter_description = Lens.lens (\CreateFindingsFilter' {description} -> description) (\s@CreateFindingsFilter' {} a -> s {description = a} :: CreateFindingsFilter)

-- | The position of the filter in the list of saved filters on the Amazon
-- Macie console. This value also determines the order in which the filter
-- is applied to findings, relative to other filters that are also applied
-- to the findings.
createFindingsFilter_position :: Lens.Lens' CreateFindingsFilter (Prelude.Maybe Prelude.Int)
createFindingsFilter_position = Lens.lens (\CreateFindingsFilter' {position} -> position) (\s@CreateFindingsFilter' {} a -> s {position = a} :: CreateFindingsFilter)

-- | A map of key-value pairs that specifies the tags to associate with the
-- filter.
--
-- A findings filter can have a maximum of 50 tags. Each tag consists of a
-- tag key and an associated tag value. The maximum length of a tag key is
-- 128 characters. The maximum length of a tag value is 256 characters.
createFindingsFilter_tags :: Lens.Lens' CreateFindingsFilter (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFindingsFilter_tags = Lens.lens (\CreateFindingsFilter' {tags} -> tags) (\s@CreateFindingsFilter' {} a -> s {tags = a} :: CreateFindingsFilter) Prelude.. Lens.mapping Lens.coerced

-- | The action to perform on findings that match the filter criteria
-- (findingCriteria). Valid values are: ARCHIVE, suppress (automatically
-- archive) the findings; and, NOOP, don\'t perform any action on the
-- findings.
createFindingsFilter_action :: Lens.Lens' CreateFindingsFilter FindingsFilterAction
createFindingsFilter_action = Lens.lens (\CreateFindingsFilter' {action} -> action) (\s@CreateFindingsFilter' {} a -> s {action = a} :: CreateFindingsFilter)

-- | The criteria to use to filter findings.
createFindingsFilter_findingCriteria :: Lens.Lens' CreateFindingsFilter FindingCriteria
createFindingsFilter_findingCriteria = Lens.lens (\CreateFindingsFilter' {findingCriteria} -> findingCriteria) (\s@CreateFindingsFilter' {} a -> s {findingCriteria = a} :: CreateFindingsFilter)

-- | A custom name for the filter. The name must contain at least 3
-- characters and can contain as many as 64 characters.
--
-- We strongly recommend that you avoid including any sensitive data in the
-- name of a filter. Other users of your account might be able to see this
-- name, depending on the actions that they\'re allowed to perform in
-- Amazon Macie.
createFindingsFilter_name :: Lens.Lens' CreateFindingsFilter Prelude.Text
createFindingsFilter_name = Lens.lens (\CreateFindingsFilter' {name} -> name) (\s@CreateFindingsFilter' {} a -> s {name = a} :: CreateFindingsFilter)

instance Core.AWSRequest CreateFindingsFilter where
  type
    AWSResponse CreateFindingsFilter =
      CreateFindingsFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFindingsFilterResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFindingsFilter where
  hashWithSalt _salt CreateFindingsFilter' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` findingCriteria
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateFindingsFilter where
  rnf CreateFindingsFilter' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf findingCriteria
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateFindingsFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFindingsFilter where
  toJSON CreateFindingsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("description" Data..=) Prelude.<$> description,
            ("position" Data..=) Prelude.<$> position,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("action" Data..= action),
            Prelude.Just
              ("findingCriteria" Data..= findingCriteria),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateFindingsFilter where
  toPath = Prelude.const "/findingsfilters"

instance Data.ToQuery CreateFindingsFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFindingsFilterResponse' smart constructor.
data CreateFindingsFilterResponse = CreateFindingsFilterResponse'
  { -- | The Amazon Resource Name (ARN) of the filter that was created.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the filter that was created.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFindingsFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createFindingsFilterResponse_arn' - The Amazon Resource Name (ARN) of the filter that was created.
--
-- 'id', 'createFindingsFilterResponse_id' - The unique identifier for the filter that was created.
--
-- 'httpStatus', 'createFindingsFilterResponse_httpStatus' - The response's http status code.
newCreateFindingsFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFindingsFilterResponse
newCreateFindingsFilterResponse pHttpStatus_ =
  CreateFindingsFilterResponse'
    { arn =
        Prelude.Nothing,
      id = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the filter that was created.
createFindingsFilterResponse_arn :: Lens.Lens' CreateFindingsFilterResponse (Prelude.Maybe Prelude.Text)
createFindingsFilterResponse_arn = Lens.lens (\CreateFindingsFilterResponse' {arn} -> arn) (\s@CreateFindingsFilterResponse' {} a -> s {arn = a} :: CreateFindingsFilterResponse)

-- | The unique identifier for the filter that was created.
createFindingsFilterResponse_id :: Lens.Lens' CreateFindingsFilterResponse (Prelude.Maybe Prelude.Text)
createFindingsFilterResponse_id = Lens.lens (\CreateFindingsFilterResponse' {id} -> id) (\s@CreateFindingsFilterResponse' {} a -> s {id = a} :: CreateFindingsFilterResponse)

-- | The response's http status code.
createFindingsFilterResponse_httpStatus :: Lens.Lens' CreateFindingsFilterResponse Prelude.Int
createFindingsFilterResponse_httpStatus = Lens.lens (\CreateFindingsFilterResponse' {httpStatus} -> httpStatus) (\s@CreateFindingsFilterResponse' {} a -> s {httpStatus = a} :: CreateFindingsFilterResponse)

instance Prelude.NFData CreateFindingsFilterResponse where
  rnf CreateFindingsFilterResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf httpStatus
