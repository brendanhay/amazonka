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
-- Module      : Amazonka.OAM.GetSink
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns complete information about one monitoring account sink.
--
-- To use this operation, provide the sink ARN. To retrieve a list of sink
-- ARNs, use
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_ListSinks.html ListSinks>.
module Amazonka.OAM.GetSink
  ( -- * Creating a Request
    GetSink (..),
    newGetSink,

    -- * Request Lenses
    getSink_identifier,

    -- * Destructuring the Response
    GetSinkResponse (..),
    newGetSinkResponse,

    -- * Response Lenses
    getSinkResponse_arn,
    getSinkResponse_id,
    getSinkResponse_name,
    getSinkResponse_tags,
    getSinkResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSink' smart constructor.
data GetSink = GetSink'
  { -- | The ARN of the sink to retrieve information for.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getSink_identifier' - The ARN of the sink to retrieve information for.
newGetSink ::
  -- | 'identifier'
  Prelude.Text ->
  GetSink
newGetSink pIdentifier_ =
  GetSink' {identifier = pIdentifier_}

-- | The ARN of the sink to retrieve information for.
getSink_identifier :: Lens.Lens' GetSink Prelude.Text
getSink_identifier = Lens.lens (\GetSink' {identifier} -> identifier) (\s@GetSink' {} a -> s {identifier = a} :: GetSink)

instance Core.AWSRequest GetSink where
  type AWSResponse GetSink = GetSinkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSinkResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Id")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSink where
  hashWithSalt _salt GetSink' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData GetSink where
  rnf GetSink' {..} = Prelude.rnf identifier

instance Data.ToHeaders GetSink where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSink where
  toJSON GetSink' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Identifier" Data..= identifier)]
      )

instance Data.ToPath GetSink where
  toPath = Prelude.const "/GetSink"

instance Data.ToQuery GetSink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSinkResponse' smart constructor.
data GetSinkResponse = GetSinkResponse'
  { -- | The ARN of the sink.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The random ID string that Amazon Web Services generated as part of the
    -- sink ARN.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the sink.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the sink.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getSinkResponse_arn' - The ARN of the sink.
--
-- 'id', 'getSinkResponse_id' - The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
--
-- 'name', 'getSinkResponse_name' - The name of the sink.
--
-- 'tags', 'getSinkResponse_tags' - The tags assigned to the sink.
--
-- 'httpStatus', 'getSinkResponse_httpStatus' - The response's http status code.
newGetSinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSinkResponse
newGetSinkResponse pHttpStatus_ =
  GetSinkResponse'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the sink.
getSinkResponse_arn :: Lens.Lens' GetSinkResponse (Prelude.Maybe Prelude.Text)
getSinkResponse_arn = Lens.lens (\GetSinkResponse' {arn} -> arn) (\s@GetSinkResponse' {} a -> s {arn = a} :: GetSinkResponse)

-- | The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
getSinkResponse_id :: Lens.Lens' GetSinkResponse (Prelude.Maybe Prelude.Text)
getSinkResponse_id = Lens.lens (\GetSinkResponse' {id} -> id) (\s@GetSinkResponse' {} a -> s {id = a} :: GetSinkResponse)

-- | The name of the sink.
getSinkResponse_name :: Lens.Lens' GetSinkResponse (Prelude.Maybe Prelude.Text)
getSinkResponse_name = Lens.lens (\GetSinkResponse' {name} -> name) (\s@GetSinkResponse' {} a -> s {name = a} :: GetSinkResponse)

-- | The tags assigned to the sink.
getSinkResponse_tags :: Lens.Lens' GetSinkResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getSinkResponse_tags = Lens.lens (\GetSinkResponse' {tags} -> tags) (\s@GetSinkResponse' {} a -> s {tags = a} :: GetSinkResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getSinkResponse_httpStatus :: Lens.Lens' GetSinkResponse Prelude.Int
getSinkResponse_httpStatus = Lens.lens (\GetSinkResponse' {httpStatus} -> httpStatus) (\s@GetSinkResponse' {} a -> s {httpStatus = a} :: GetSinkResponse)

instance Prelude.NFData GetSinkResponse where
  rnf GetSinkResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf httpStatus
