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
-- Module      : Amazonka.FraudDetector.GetListElements
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the elements in the specified list.
module Amazonka.FraudDetector.GetListElements
  ( -- * Creating a Request
    GetListElements (..),
    newGetListElements,

    -- * Request Lenses
    getListElements_maxResults,
    getListElements_nextToken,
    getListElements_name,

    -- * Destructuring the Response
    GetListElementsResponse (..),
    newGetListElementsResponse,

    -- * Response Lenses
    getListElementsResponse_elements,
    getListElementsResponse_nextToken,
    getListElementsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetListElements' smart constructor.
data GetListElements = GetListElements'
  { -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the list.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetListElements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getListElements_maxResults' - The maximum number of objects to return for the request.
--
-- 'nextToken', 'getListElements_nextToken' - The next token for the subsequent request.
--
-- 'name', 'getListElements_name' - The name of the list.
newGetListElements ::
  -- | 'name'
  Prelude.Text ->
  GetListElements
newGetListElements pName_ =
  GetListElements'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of objects to return for the request.
getListElements_maxResults :: Lens.Lens' GetListElements (Prelude.Maybe Prelude.Natural)
getListElements_maxResults = Lens.lens (\GetListElements' {maxResults} -> maxResults) (\s@GetListElements' {} a -> s {maxResults = a} :: GetListElements)

-- | The next token for the subsequent request.
getListElements_nextToken :: Lens.Lens' GetListElements (Prelude.Maybe Prelude.Text)
getListElements_nextToken = Lens.lens (\GetListElements' {nextToken} -> nextToken) (\s@GetListElements' {} a -> s {nextToken = a} :: GetListElements)

-- | The name of the list.
getListElements_name :: Lens.Lens' GetListElements Prelude.Text
getListElements_name = Lens.lens (\GetListElements' {name} -> name) (\s@GetListElements' {} a -> s {name = a} :: GetListElements)

instance Core.AWSRequest GetListElements where
  type
    AWSResponse GetListElements =
      GetListElementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetListElementsResponse'
            Prelude.<$> (x Data..?> "elements" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetListElements where
  hashWithSalt _salt GetListElements' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetListElements where
  rnf GetListElements' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetListElements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetListElements" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetListElements where
  toJSON GetListElements' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath GetListElements where
  toPath = Prelude.const "/"

instance Data.ToQuery GetListElements where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetListElementsResponse' smart constructor.
data GetListElementsResponse = GetListElementsResponse'
  { -- | The list elements.
    elements :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetListElementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elements', 'getListElementsResponse_elements' - The list elements.
--
-- 'nextToken', 'getListElementsResponse_nextToken' - The next page token.
--
-- 'httpStatus', 'getListElementsResponse_httpStatus' - The response's http status code.
newGetListElementsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetListElementsResponse
newGetListElementsResponse pHttpStatus_ =
  GetListElementsResponse'
    { elements =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list elements.
getListElementsResponse_elements :: Lens.Lens' GetListElementsResponse (Prelude.Maybe [Prelude.Text])
getListElementsResponse_elements = Lens.lens (\GetListElementsResponse' {elements} -> elements) (\s@GetListElementsResponse' {} a -> s {elements = a} :: GetListElementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token.
getListElementsResponse_nextToken :: Lens.Lens' GetListElementsResponse (Prelude.Maybe Prelude.Text)
getListElementsResponse_nextToken = Lens.lens (\GetListElementsResponse' {nextToken} -> nextToken) (\s@GetListElementsResponse' {} a -> s {nextToken = a} :: GetListElementsResponse)

-- | The response's http status code.
getListElementsResponse_httpStatus :: Lens.Lens' GetListElementsResponse Prelude.Int
getListElementsResponse_httpStatus = Lens.lens (\GetListElementsResponse' {httpStatus} -> httpStatus) (\s@GetListElementsResponse' {} a -> s {httpStatus = a} :: GetListElementsResponse)

instance Prelude.NFData GetListElementsResponse where
  rnf GetListElementsResponse' {..} =
    Prelude.rnf elements
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
