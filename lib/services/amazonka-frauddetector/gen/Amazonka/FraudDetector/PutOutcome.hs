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
-- Module      : Amazonka.FraudDetector.PutOutcome
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an outcome.
module Amazonka.FraudDetector.PutOutcome
  ( -- * Creating a Request
    PutOutcome (..),
    newPutOutcome,

    -- * Request Lenses
    putOutcome_tags,
    putOutcome_description,
    putOutcome_name,

    -- * Destructuring the Response
    PutOutcomeResponse (..),
    newPutOutcomeResponse,

    -- * Response Lenses
    putOutcomeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutOutcome' smart constructor.
data PutOutcome = PutOutcome'
  { -- | A collection of key and value pairs.
    tags :: Prelude.Maybe [Tag],
    -- | The outcome description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the outcome.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOutcome' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putOutcome_tags' - A collection of key and value pairs.
--
-- 'description', 'putOutcome_description' - The outcome description.
--
-- 'name', 'putOutcome_name' - The name of the outcome.
newPutOutcome ::
  -- | 'name'
  Prelude.Text ->
  PutOutcome
newPutOutcome pName_ =
  PutOutcome'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | A collection of key and value pairs.
putOutcome_tags :: Lens.Lens' PutOutcome (Prelude.Maybe [Tag])
putOutcome_tags = Lens.lens (\PutOutcome' {tags} -> tags) (\s@PutOutcome' {} a -> s {tags = a} :: PutOutcome) Prelude.. Lens.mapping Lens.coerced

-- | The outcome description.
putOutcome_description :: Lens.Lens' PutOutcome (Prelude.Maybe Prelude.Text)
putOutcome_description = Lens.lens (\PutOutcome' {description} -> description) (\s@PutOutcome' {} a -> s {description = a} :: PutOutcome)

-- | The name of the outcome.
putOutcome_name :: Lens.Lens' PutOutcome Prelude.Text
putOutcome_name = Lens.lens (\PutOutcome' {name} -> name) (\s@PutOutcome' {} a -> s {name = a} :: PutOutcome)

instance Core.AWSRequest PutOutcome where
  type AWSResponse PutOutcome = PutOutcomeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutOutcomeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutOutcome where
  hashWithSalt _salt PutOutcome' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData PutOutcome where
  rnf PutOutcome' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders PutOutcome where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.PutOutcome" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutOutcome where
  toJSON PutOutcome' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath PutOutcome where
  toPath = Prelude.const "/"

instance Data.ToQuery PutOutcome where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutOutcomeResponse' smart constructor.
data PutOutcomeResponse = PutOutcomeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutOutcomeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putOutcomeResponse_httpStatus' - The response's http status code.
newPutOutcomeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutOutcomeResponse
newPutOutcomeResponse pHttpStatus_ =
  PutOutcomeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putOutcomeResponse_httpStatus :: Lens.Lens' PutOutcomeResponse Prelude.Int
putOutcomeResponse_httpStatus = Lens.lens (\PutOutcomeResponse' {httpStatus} -> httpStatus) (\s@PutOutcomeResponse' {} a -> s {httpStatus = a} :: PutOutcomeResponse)

instance Prelude.NFData PutOutcomeResponse where
  rnf PutOutcomeResponse' {..} = Prelude.rnf httpStatus
