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
-- Module      : Amazonka.ServiceCatalog.DescribeTagOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified TagOption.
module Amazonka.ServiceCatalog.DescribeTagOption
  ( -- * Creating a Request
    DescribeTagOption (..),
    newDescribeTagOption,

    -- * Request Lenses
    describeTagOption_id,

    -- * Destructuring the Response
    DescribeTagOptionResponse (..),
    newDescribeTagOptionResponse,

    -- * Response Lenses
    describeTagOptionResponse_tagOptionDetail,
    describeTagOptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeTagOption' smart constructor.
data DescribeTagOption = DescribeTagOption'
  { -- | The TagOption identifier.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTagOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeTagOption_id' - The TagOption identifier.
newDescribeTagOption ::
  -- | 'id'
  Prelude.Text ->
  DescribeTagOption
newDescribeTagOption pId_ =
  DescribeTagOption' {id = pId_}

-- | The TagOption identifier.
describeTagOption_id :: Lens.Lens' DescribeTagOption Prelude.Text
describeTagOption_id = Lens.lens (\DescribeTagOption' {id} -> id) (\s@DescribeTagOption' {} a -> s {id = a} :: DescribeTagOption)

instance Core.AWSRequest DescribeTagOption where
  type
    AWSResponse DescribeTagOption =
      DescribeTagOptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTagOptionResponse'
            Prelude.<$> (x Data..?> "TagOptionDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTagOption where
  hashWithSalt _salt DescribeTagOption' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeTagOption where
  rnf DescribeTagOption' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeTagOption where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DescribeTagOption" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTagOption where
  toJSON DescribeTagOption' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("Id" Data..= id)])

instance Data.ToPath DescribeTagOption where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTagOption where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTagOptionResponse' smart constructor.
data DescribeTagOptionResponse = DescribeTagOptionResponse'
  { -- | Information about the TagOption.
    tagOptionDetail :: Prelude.Maybe TagOptionDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTagOptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagOptionDetail', 'describeTagOptionResponse_tagOptionDetail' - Information about the TagOption.
--
-- 'httpStatus', 'describeTagOptionResponse_httpStatus' - The response's http status code.
newDescribeTagOptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTagOptionResponse
newDescribeTagOptionResponse pHttpStatus_ =
  DescribeTagOptionResponse'
    { tagOptionDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the TagOption.
describeTagOptionResponse_tagOptionDetail :: Lens.Lens' DescribeTagOptionResponse (Prelude.Maybe TagOptionDetail)
describeTagOptionResponse_tagOptionDetail = Lens.lens (\DescribeTagOptionResponse' {tagOptionDetail} -> tagOptionDetail) (\s@DescribeTagOptionResponse' {} a -> s {tagOptionDetail = a} :: DescribeTagOptionResponse)

-- | The response's http status code.
describeTagOptionResponse_httpStatus :: Lens.Lens' DescribeTagOptionResponse Prelude.Int
describeTagOptionResponse_httpStatus = Lens.lens (\DescribeTagOptionResponse' {httpStatus} -> httpStatus) (\s@DescribeTagOptionResponse' {} a -> s {httpStatus = a} :: DescribeTagOptionResponse)

instance Prelude.NFData DescribeTagOptionResponse where
  rnf DescribeTagOptionResponse' {..} =
    Prelude.rnf tagOptionDetail
      `Prelude.seq` Prelude.rnf httpStatus
