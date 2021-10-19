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
-- Module      : Network.AWS.WAF.ListLoggingConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListLoggingConfigurations
  ( -- * Creating a Request
    ListLoggingConfigurations (..),
    newListLoggingConfigurations,

    -- * Request Lenses
    listLoggingConfigurations_nextMarker,
    listLoggingConfigurations_limit,

    -- * Destructuring the Response
    ListLoggingConfigurationsResponse (..),
    newListLoggingConfigurationsResponse,

    -- * Response Lenses
    listLoggingConfigurationsResponse_nextMarker,
    listLoggingConfigurationsResponse_loggingConfigurations,
    listLoggingConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newListLoggingConfigurations' smart constructor.
data ListLoggingConfigurations = ListLoggingConfigurations'
  { -- | If you specify a value for @Limit@ and you have more
    -- @LoggingConfigurations@ than the value of @Limit@, AWS WAF returns a
    -- @NextMarker@ value in the response that allows you to list another group
    -- of @LoggingConfigurations@. For the second and subsequent
    -- @ListLoggingConfigurations@ requests, specify the value of @NextMarker@
    -- from the previous response to get information about another batch of
    -- @ListLoggingConfigurations@.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @LoggingConfigurations@ that you want AWS WAF to
    -- return for this request. If you have more @LoggingConfigurations@ than
    -- the number that you specify for @Limit@, the response includes a
    -- @NextMarker@ value that you can use to get another batch of
    -- @LoggingConfigurations@.
    limit :: Prelude.Maybe Prelude.Natural
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
-- 'nextMarker', 'listLoggingConfigurations_nextMarker' - If you specify a value for @Limit@ and you have more
-- @LoggingConfigurations@ than the value of @Limit@, AWS WAF returns a
-- @NextMarker@ value in the response that allows you to list another group
-- of @LoggingConfigurations@. For the second and subsequent
-- @ListLoggingConfigurations@ requests, specify the value of @NextMarker@
-- from the previous response to get information about another batch of
-- @ListLoggingConfigurations@.
--
-- 'limit', 'listLoggingConfigurations_limit' - Specifies the number of @LoggingConfigurations@ that you want AWS WAF to
-- return for this request. If you have more @LoggingConfigurations@ than
-- the number that you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @LoggingConfigurations@.
newListLoggingConfigurations ::
  ListLoggingConfigurations
newListLoggingConfigurations =
  ListLoggingConfigurations'
    { nextMarker =
        Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more
-- @LoggingConfigurations@ than the value of @Limit@, AWS WAF returns a
-- @NextMarker@ value in the response that allows you to list another group
-- of @LoggingConfigurations@. For the second and subsequent
-- @ListLoggingConfigurations@ requests, specify the value of @NextMarker@
-- from the previous response to get information about another batch of
-- @ListLoggingConfigurations@.
listLoggingConfigurations_nextMarker :: Lens.Lens' ListLoggingConfigurations (Prelude.Maybe Prelude.Text)
listLoggingConfigurations_nextMarker = Lens.lens (\ListLoggingConfigurations' {nextMarker} -> nextMarker) (\s@ListLoggingConfigurations' {} a -> s {nextMarker = a} :: ListLoggingConfigurations)

-- | Specifies the number of @LoggingConfigurations@ that you want AWS WAF to
-- return for this request. If you have more @LoggingConfigurations@ than
-- the number that you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @LoggingConfigurations@.
listLoggingConfigurations_limit :: Lens.Lens' ListLoggingConfigurations (Prelude.Maybe Prelude.Natural)
listLoggingConfigurations_limit = Lens.lens (\ListLoggingConfigurations' {limit} -> limit) (\s@ListLoggingConfigurations' {} a -> s {limit = a} :: ListLoggingConfigurations)

instance Core.AWSPager ListLoggingConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLoggingConfigurationsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLoggingConfigurationsResponse_loggingConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLoggingConfigurations_nextMarker
          Lens..~ rs
          Lens.^? listLoggingConfigurationsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListLoggingConfigurations where
  type
    AWSResponse ListLoggingConfigurations =
      ListLoggingConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLoggingConfigurationsResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> ( x Core..?> "LoggingConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLoggingConfigurations

instance Prelude.NFData ListLoggingConfigurations

instance Core.ToHeaders ListLoggingConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.ListLoggingConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLoggingConfigurations where
  toJSON ListLoggingConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextMarker" Core..=) Prelude.<$> nextMarker,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListLoggingConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListLoggingConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLoggingConfigurationsResponse' smart constructor.
data ListLoggingConfigurationsResponse = ListLoggingConfigurationsResponse'
  { -- | If you have more @LoggingConfigurations@ than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @LoggingConfigurations@, submit another
    -- @ListLoggingConfigurations@ request, and specify the @NextMarker@ value
    -- from the response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of LoggingConfiguration objects.
    loggingConfigurations :: Prelude.Maybe [LoggingConfiguration],
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
-- 'nextMarker', 'listLoggingConfigurationsResponse_nextMarker' - If you have more @LoggingConfigurations@ than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @LoggingConfigurations@, submit another
-- @ListLoggingConfigurations@ request, and specify the @NextMarker@ value
-- from the response in the @NextMarker@ value in the next request.
--
-- 'loggingConfigurations', 'listLoggingConfigurationsResponse_loggingConfigurations' - An array of LoggingConfiguration objects.
--
-- 'httpStatus', 'listLoggingConfigurationsResponse_httpStatus' - The response's http status code.
newListLoggingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLoggingConfigurationsResponse
newListLoggingConfigurationsResponse pHttpStatus_ =
  ListLoggingConfigurationsResponse'
    { nextMarker =
        Prelude.Nothing,
      loggingConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @LoggingConfigurations@ than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @LoggingConfigurations@, submit another
-- @ListLoggingConfigurations@ request, and specify the @NextMarker@ value
-- from the response in the @NextMarker@ value in the next request.
listLoggingConfigurationsResponse_nextMarker :: Lens.Lens' ListLoggingConfigurationsResponse (Prelude.Maybe Prelude.Text)
listLoggingConfigurationsResponse_nextMarker = Lens.lens (\ListLoggingConfigurationsResponse' {nextMarker} -> nextMarker) (\s@ListLoggingConfigurationsResponse' {} a -> s {nextMarker = a} :: ListLoggingConfigurationsResponse)

-- | An array of LoggingConfiguration objects.
listLoggingConfigurationsResponse_loggingConfigurations :: Lens.Lens' ListLoggingConfigurationsResponse (Prelude.Maybe [LoggingConfiguration])
listLoggingConfigurationsResponse_loggingConfigurations = Lens.lens (\ListLoggingConfigurationsResponse' {loggingConfigurations} -> loggingConfigurations) (\s@ListLoggingConfigurationsResponse' {} a -> s {loggingConfigurations = a} :: ListLoggingConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLoggingConfigurationsResponse_httpStatus :: Lens.Lens' ListLoggingConfigurationsResponse Prelude.Int
listLoggingConfigurationsResponse_httpStatus = Lens.lens (\ListLoggingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListLoggingConfigurationsResponse' {} a -> s {httpStatus = a} :: ListLoggingConfigurationsResponse)

instance
  Prelude.NFData
    ListLoggingConfigurationsResponse
