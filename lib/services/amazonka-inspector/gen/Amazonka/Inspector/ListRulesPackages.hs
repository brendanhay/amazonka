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
-- Module      : Amazonka.Inspector.ListRulesPackages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available Amazon Inspector rules packages.
--
-- This operation returns paginated results.
module Amazonka.Inspector.ListRulesPackages
  ( -- * Creating a Request
    ListRulesPackages (..),
    newListRulesPackages,

    -- * Request Lenses
    listRulesPackages_nextToken,
    listRulesPackages_maxResults,

    -- * Destructuring the Response
    ListRulesPackagesResponse (..),
    newListRulesPackagesResponse,

    -- * Response Lenses
    listRulesPackagesResponse_nextToken,
    listRulesPackagesResponse_httpStatus,
    listRulesPackagesResponse_rulesPackageArns,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRulesPackages' smart constructor.
data ListRulesPackages = ListRulesPackages'
  { -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the __ListRulesPackages__
    -- action. Subsequent calls to the action fill __nextToken__ in the request
    -- with the value of __NextToken__ from the previous response to continue
    -- listing data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can use this parameter to indicate the maximum number of items you
    -- want in the response. The default value is 10. The maximum value is 500.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRulesPackages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRulesPackages_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the __ListRulesPackages__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from the previous response to continue
-- listing data.
--
-- 'maxResults', 'listRulesPackages_maxResults' - You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
newListRulesPackages ::
  ListRulesPackages
newListRulesPackages =
  ListRulesPackages'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the __ListRulesPackages__
-- action. Subsequent calls to the action fill __nextToken__ in the request
-- with the value of __NextToken__ from the previous response to continue
-- listing data.
listRulesPackages_nextToken :: Lens.Lens' ListRulesPackages (Prelude.Maybe Prelude.Text)
listRulesPackages_nextToken = Lens.lens (\ListRulesPackages' {nextToken} -> nextToken) (\s@ListRulesPackages' {} a -> s {nextToken = a} :: ListRulesPackages)

-- | You can use this parameter to indicate the maximum number of items you
-- want in the response. The default value is 10. The maximum value is 500.
listRulesPackages_maxResults :: Lens.Lens' ListRulesPackages (Prelude.Maybe Prelude.Int)
listRulesPackages_maxResults = Lens.lens (\ListRulesPackages' {maxResults} -> maxResults) (\s@ListRulesPackages' {} a -> s {maxResults = a} :: ListRulesPackages)

instance Core.AWSPager ListRulesPackages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesPackagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listRulesPackagesResponse_rulesPackageArns
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRulesPackages_nextToken
          Lens..~ rs
          Lens.^? listRulesPackagesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRulesPackages where
  type
    AWSResponse ListRulesPackages =
      ListRulesPackagesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesPackagesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "rulesPackageArns"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListRulesPackages where
  hashWithSalt _salt ListRulesPackages' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListRulesPackages where
  rnf ListRulesPackages' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListRulesPackages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "InspectorService.ListRulesPackages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRulesPackages where
  toJSON ListRulesPackages' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListRulesPackages where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRulesPackages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRulesPackagesResponse' smart constructor.
data ListRulesPackagesResponse = ListRulesPackagesResponse'
  { -- | When a response is generated, if there is more data to be listed, this
    -- parameter is present in the response and contains the value to use for
    -- the __nextToken__ parameter in a subsequent pagination request. If there
    -- is no more data to be listed, this parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of ARNs that specifies the rules packages returned by the
    -- action.
    rulesPackageArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRulesPackagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRulesPackagesResponse_nextToken' - When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
--
-- 'httpStatus', 'listRulesPackagesResponse_httpStatus' - The response's http status code.
--
-- 'rulesPackageArns', 'listRulesPackagesResponse_rulesPackageArns' - The list of ARNs that specifies the rules packages returned by the
-- action.
newListRulesPackagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRulesPackagesResponse
newListRulesPackagesResponse pHttpStatus_ =
  ListRulesPackagesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      rulesPackageArns = Prelude.mempty
    }

-- | When a response is generated, if there is more data to be listed, this
-- parameter is present in the response and contains the value to use for
-- the __nextToken__ parameter in a subsequent pagination request. If there
-- is no more data to be listed, this parameter is set to null.
listRulesPackagesResponse_nextToken :: Lens.Lens' ListRulesPackagesResponse (Prelude.Maybe Prelude.Text)
listRulesPackagesResponse_nextToken = Lens.lens (\ListRulesPackagesResponse' {nextToken} -> nextToken) (\s@ListRulesPackagesResponse' {} a -> s {nextToken = a} :: ListRulesPackagesResponse)

-- | The response's http status code.
listRulesPackagesResponse_httpStatus :: Lens.Lens' ListRulesPackagesResponse Prelude.Int
listRulesPackagesResponse_httpStatus = Lens.lens (\ListRulesPackagesResponse' {httpStatus} -> httpStatus) (\s@ListRulesPackagesResponse' {} a -> s {httpStatus = a} :: ListRulesPackagesResponse)

-- | The list of ARNs that specifies the rules packages returned by the
-- action.
listRulesPackagesResponse_rulesPackageArns :: Lens.Lens' ListRulesPackagesResponse [Prelude.Text]
listRulesPackagesResponse_rulesPackageArns = Lens.lens (\ListRulesPackagesResponse' {rulesPackageArns} -> rulesPackageArns) (\s@ListRulesPackagesResponse' {} a -> s {rulesPackageArns = a} :: ListRulesPackagesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRulesPackagesResponse where
  rnf ListRulesPackagesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf rulesPackageArns
