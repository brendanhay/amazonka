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
-- Module      : Amazonka.WAFRegional.ListLoggingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Returns an array of LoggingConfiguration objects.
module Amazonka.WAFRegional.ListLoggingConfigurations
  ( -- * Creating a Request
    ListLoggingConfigurations (..),
    newListLoggingConfigurations,

    -- * Request Lenses
    listLoggingConfigurations_limit,
    listLoggingConfigurations_nextMarker,

    -- * Destructuring the Response
    ListLoggingConfigurationsResponse (..),
    newListLoggingConfigurationsResponse,

    -- * Response Lenses
    listLoggingConfigurationsResponse_loggingConfigurations,
    listLoggingConfigurationsResponse_nextMarker,
    listLoggingConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newListLoggingConfigurations' smart constructor.
data ListLoggingConfigurations = ListLoggingConfigurations'
  { -- | Specifies the number of @LoggingConfigurations@ that you want AWS WAF to
    -- return for this request. If you have more @LoggingConfigurations@ than
    -- the number that you specify for @Limit@, the response includes a
    -- @NextMarker@ value that you can use to get another batch of
    -- @LoggingConfigurations@.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more
    -- @LoggingConfigurations@ than the value of @Limit@, AWS WAF returns a
    -- @NextMarker@ value in the response that allows you to list another group
    -- of @LoggingConfigurations@. For the second and subsequent
    -- @ListLoggingConfigurations@ requests, specify the value of @NextMarker@
    -- from the previous response to get information about another batch of
    -- @ListLoggingConfigurations@.
    nextMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLoggingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listLoggingConfigurations_limit' - Specifies the number of @LoggingConfigurations@ that you want AWS WAF to
-- return for this request. If you have more @LoggingConfigurations@ than
-- the number that you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @LoggingConfigurations@.
--
-- 'nextMarker', 'listLoggingConfigurations_nextMarker' - If you specify a value for @Limit@ and you have more
-- @LoggingConfigurations@ than the value of @Limit@, AWS WAF returns a
-- @NextMarker@ value in the response that allows you to list another group
-- of @LoggingConfigurations@. For the second and subsequent
-- @ListLoggingConfigurations@ requests, specify the value of @NextMarker@
-- from the previous response to get information about another batch of
-- @ListLoggingConfigurations@.
newListLoggingConfigurations ::
  ListLoggingConfigurations
newListLoggingConfigurations =
  ListLoggingConfigurations'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | Specifies the number of @LoggingConfigurations@ that you want AWS WAF to
-- return for this request. If you have more @LoggingConfigurations@ than
-- the number that you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @LoggingConfigurations@.
listLoggingConfigurations_limit :: Lens.Lens' ListLoggingConfigurations (Prelude.Maybe Prelude.Natural)
listLoggingConfigurations_limit = Lens.lens (\ListLoggingConfigurations' {limit} -> limit) (\s@ListLoggingConfigurations' {} a -> s {limit = a} :: ListLoggingConfigurations)

-- | If you specify a value for @Limit@ and you have more
-- @LoggingConfigurations@ than the value of @Limit@, AWS WAF returns a
-- @NextMarker@ value in the response that allows you to list another group
-- of @LoggingConfigurations@. For the second and subsequent
-- @ListLoggingConfigurations@ requests, specify the value of @NextMarker@
-- from the previous response to get information about another batch of
-- @ListLoggingConfigurations@.
listLoggingConfigurations_nextMarker :: Lens.Lens' ListLoggingConfigurations (Prelude.Maybe Prelude.Text)
listLoggingConfigurations_nextMarker = Lens.lens (\ListLoggingConfigurations' {nextMarker} -> nextMarker) (\s@ListLoggingConfigurations' {} a -> s {nextMarker = a} :: ListLoggingConfigurations)

instance Core.AWSRequest ListLoggingConfigurations where
  type
    AWSResponse ListLoggingConfigurations =
      ListLoggingConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLoggingConfigurationsResponse'
            Prelude.<$> ( x Data..?> "LoggingConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLoggingConfigurations where
  hashWithSalt _salt ListLoggingConfigurations' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListLoggingConfigurations where
  rnf ListLoggingConfigurations' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Data.ToHeaders ListLoggingConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.ListLoggingConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLoggingConfigurations where
  toJSON ListLoggingConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker
          ]
      )

instance Data.ToPath ListLoggingConfigurations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLoggingConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLoggingConfigurationsResponse' smart constructor.
data ListLoggingConfigurationsResponse = ListLoggingConfigurationsResponse'
  { -- | An array of LoggingConfiguration objects.
    loggingConfigurations :: Prelude.Maybe [LoggingConfiguration],
    -- | If you have more @LoggingConfigurations@ than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @LoggingConfigurations@, submit another
    -- @ListLoggingConfigurations@ request, and specify the @NextMarker@ value
    -- from the response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLoggingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfigurations', 'listLoggingConfigurationsResponse_loggingConfigurations' - An array of LoggingConfiguration objects.
--
-- 'nextMarker', 'listLoggingConfigurationsResponse_nextMarker' - If you have more @LoggingConfigurations@ than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @LoggingConfigurations@, submit another
-- @ListLoggingConfigurations@ request, and specify the @NextMarker@ value
-- from the response in the @NextMarker@ value in the next request.
--
-- 'httpStatus', 'listLoggingConfigurationsResponse_httpStatus' - The response's http status code.
newListLoggingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLoggingConfigurationsResponse
newListLoggingConfigurationsResponse pHttpStatus_ =
  ListLoggingConfigurationsResponse'
    { loggingConfigurations =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of LoggingConfiguration objects.
listLoggingConfigurationsResponse_loggingConfigurations :: Lens.Lens' ListLoggingConfigurationsResponse (Prelude.Maybe [LoggingConfiguration])
listLoggingConfigurationsResponse_loggingConfigurations = Lens.lens (\ListLoggingConfigurationsResponse' {loggingConfigurations} -> loggingConfigurations) (\s@ListLoggingConfigurationsResponse' {} a -> s {loggingConfigurations = a} :: ListLoggingConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you have more @LoggingConfigurations@ than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @LoggingConfigurations@, submit another
-- @ListLoggingConfigurations@ request, and specify the @NextMarker@ value
-- from the response in the @NextMarker@ value in the next request.
listLoggingConfigurationsResponse_nextMarker :: Lens.Lens' ListLoggingConfigurationsResponse (Prelude.Maybe Prelude.Text)
listLoggingConfigurationsResponse_nextMarker = Lens.lens (\ListLoggingConfigurationsResponse' {nextMarker} -> nextMarker) (\s@ListLoggingConfigurationsResponse' {} a -> s {nextMarker = a} :: ListLoggingConfigurationsResponse)

-- | The response's http status code.
listLoggingConfigurationsResponse_httpStatus :: Lens.Lens' ListLoggingConfigurationsResponse Prelude.Int
listLoggingConfigurationsResponse_httpStatus = Lens.lens (\ListLoggingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListLoggingConfigurationsResponse' {} a -> s {httpStatus = a} :: ListLoggingConfigurationsResponse)

instance
  Prelude.NFData
    ListLoggingConfigurationsResponse
  where
  rnf ListLoggingConfigurationsResponse' {..} =
    Prelude.rnf loggingConfigurations
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
