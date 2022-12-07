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
-- Module      : Amazonka.Glue.CreateCustomEntityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom pattern that is used to detect sensitive data across
-- the columns and rows of your structured data.
--
-- Each custom pattern you create specifies a regular expression and an
-- optional list of context words. If no context words are passed only a
-- regular expression is checked.
module Amazonka.Glue.CreateCustomEntityType
  ( -- * Creating a Request
    CreateCustomEntityType (..),
    newCreateCustomEntityType,

    -- * Request Lenses
    createCustomEntityType_contextWords,
    createCustomEntityType_name,
    createCustomEntityType_regexString,

    -- * Destructuring the Response
    CreateCustomEntityTypeResponse (..),
    newCreateCustomEntityTypeResponse,

    -- * Response Lenses
    createCustomEntityTypeResponse_name,
    createCustomEntityTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomEntityType' smart constructor.
data CreateCustomEntityType = CreateCustomEntityType'
  { -- | A list of context words. If none of these context words are found within
    -- the vicinity of the regular expression the data will not be detected as
    -- sensitive data.
    --
    -- If no context words are passed only a regular expression is checked.
    contextWords :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A name for the custom pattern that allows it to be retrieved or deleted
    -- later. This name must be unique per Amazon Web Services account.
    name :: Prelude.Text,
    -- | A regular expression string that is used for detecting sensitive data in
    -- a custom pattern.
    regexString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomEntityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextWords', 'createCustomEntityType_contextWords' - A list of context words. If none of these context words are found within
-- the vicinity of the regular expression the data will not be detected as
-- sensitive data.
--
-- If no context words are passed only a regular expression is checked.
--
-- 'name', 'createCustomEntityType_name' - A name for the custom pattern that allows it to be retrieved or deleted
-- later. This name must be unique per Amazon Web Services account.
--
-- 'regexString', 'createCustomEntityType_regexString' - A regular expression string that is used for detecting sensitive data in
-- a custom pattern.
newCreateCustomEntityType ::
  -- | 'name'
  Prelude.Text ->
  -- | 'regexString'
  Prelude.Text ->
  CreateCustomEntityType
newCreateCustomEntityType pName_ pRegexString_ =
  CreateCustomEntityType'
    { contextWords =
        Prelude.Nothing,
      name = pName_,
      regexString = pRegexString_
    }

-- | A list of context words. If none of these context words are found within
-- the vicinity of the regular expression the data will not be detected as
-- sensitive data.
--
-- If no context words are passed only a regular expression is checked.
createCustomEntityType_contextWords :: Lens.Lens' CreateCustomEntityType (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createCustomEntityType_contextWords = Lens.lens (\CreateCustomEntityType' {contextWords} -> contextWords) (\s@CreateCustomEntityType' {} a -> s {contextWords = a} :: CreateCustomEntityType) Prelude.. Lens.mapping Lens.coerced

-- | A name for the custom pattern that allows it to be retrieved or deleted
-- later. This name must be unique per Amazon Web Services account.
createCustomEntityType_name :: Lens.Lens' CreateCustomEntityType Prelude.Text
createCustomEntityType_name = Lens.lens (\CreateCustomEntityType' {name} -> name) (\s@CreateCustomEntityType' {} a -> s {name = a} :: CreateCustomEntityType)

-- | A regular expression string that is used for detecting sensitive data in
-- a custom pattern.
createCustomEntityType_regexString :: Lens.Lens' CreateCustomEntityType Prelude.Text
createCustomEntityType_regexString = Lens.lens (\CreateCustomEntityType' {regexString} -> regexString) (\s@CreateCustomEntityType' {} a -> s {regexString = a} :: CreateCustomEntityType)

instance Core.AWSRequest CreateCustomEntityType where
  type
    AWSResponse CreateCustomEntityType =
      CreateCustomEntityTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomEntityTypeResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCustomEntityType where
  hashWithSalt _salt CreateCustomEntityType' {..} =
    _salt `Prelude.hashWithSalt` contextWords
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` regexString

instance Prelude.NFData CreateCustomEntityType where
  rnf CreateCustomEntityType' {..} =
    Prelude.rnf contextWords
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regexString

instance Data.ToHeaders CreateCustomEntityType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.CreateCustomEntityType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCustomEntityType where
  toJSON CreateCustomEntityType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContextWords" Data..=) Prelude.<$> contextWords,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RegexString" Data..= regexString)
          ]
      )

instance Data.ToPath CreateCustomEntityType where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCustomEntityType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCustomEntityTypeResponse' smart constructor.
data CreateCustomEntityTypeResponse = CreateCustomEntityTypeResponse'
  { -- | The name of the custom pattern you created.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomEntityTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createCustomEntityTypeResponse_name' - The name of the custom pattern you created.
--
-- 'httpStatus', 'createCustomEntityTypeResponse_httpStatus' - The response's http status code.
newCreateCustomEntityTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomEntityTypeResponse
newCreateCustomEntityTypeResponse pHttpStatus_ =
  CreateCustomEntityTypeResponse'
    { name =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the custom pattern you created.
createCustomEntityTypeResponse_name :: Lens.Lens' CreateCustomEntityTypeResponse (Prelude.Maybe Prelude.Text)
createCustomEntityTypeResponse_name = Lens.lens (\CreateCustomEntityTypeResponse' {name} -> name) (\s@CreateCustomEntityTypeResponse' {} a -> s {name = a} :: CreateCustomEntityTypeResponse)

-- | The response's http status code.
createCustomEntityTypeResponse_httpStatus :: Lens.Lens' CreateCustomEntityTypeResponse Prelude.Int
createCustomEntityTypeResponse_httpStatus = Lens.lens (\CreateCustomEntityTypeResponse' {httpStatus} -> httpStatus) (\s@CreateCustomEntityTypeResponse' {} a -> s {httpStatus = a} :: CreateCustomEntityTypeResponse)

instance
  Prelude.NFData
    CreateCustomEntityTypeResponse
  where
  rnf CreateCustomEntityTypeResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
