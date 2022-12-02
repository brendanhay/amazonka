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
-- Module      : Amazonka.WAF.GetWebACL
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Returns the WebACL that is specified by @WebACLId@.
module Amazonka.WAF.GetWebACL
  ( -- * Creating a Request
    GetWebACL (..),
    newGetWebACL,

    -- * Request Lenses
    getWebACL_webACLId,

    -- * Destructuring the Response
    GetWebACLResponse (..),
    newGetWebACLResponse,

    -- * Response Lenses
    getWebACLResponse_webACL,
    getWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newGetWebACL' smart constructor.
data GetWebACL = GetWebACL'
  { -- | The @WebACLId@ of the WebACL that you want to get. @WebACLId@ is
    -- returned by CreateWebACL and by ListWebACLs.
    webACLId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACLId', 'getWebACL_webACLId' - The @WebACLId@ of the WebACL that you want to get. @WebACLId@ is
-- returned by CreateWebACL and by ListWebACLs.
newGetWebACL ::
  -- | 'webACLId'
  Prelude.Text ->
  GetWebACL
newGetWebACL pWebACLId_ =
  GetWebACL' {webACLId = pWebACLId_}

-- | The @WebACLId@ of the WebACL that you want to get. @WebACLId@ is
-- returned by CreateWebACL and by ListWebACLs.
getWebACL_webACLId :: Lens.Lens' GetWebACL Prelude.Text
getWebACL_webACLId = Lens.lens (\GetWebACL' {webACLId} -> webACLId) (\s@GetWebACL' {} a -> s {webACLId = a} :: GetWebACL)

instance Core.AWSRequest GetWebACL where
  type AWSResponse GetWebACL = GetWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWebACLResponse'
            Prelude.<$> (x Data..?> "WebACL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWebACL where
  hashWithSalt _salt GetWebACL' {..} =
    _salt `Prelude.hashWithSalt` webACLId

instance Prelude.NFData GetWebACL where
  rnf GetWebACL' {..} = Prelude.rnf webACLId

instance Data.ToHeaders GetWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSWAF_20150824.GetWebACL" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetWebACL where
  toJSON GetWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("WebACLId" Data..= webACLId)]
      )

instance Data.ToPath GetWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery GetWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWebACLResponse' smart constructor.
data GetWebACLResponse = GetWebACLResponse'
  { -- | Information about the WebACL that you specified in the @GetWebACL@
    -- request. For more information, see the following topics:
    --
    -- -   WebACL: Contains @DefaultAction@, @MetricName@, @Name@, an array of
    --     @Rule@ objects, and @WebACLId@
    --
    -- -   @DefaultAction@ (Data type is WafAction): Contains @Type@
    --
    -- -   @Rules@: Contains an array of @ActivatedRule@ objects, which contain
    --     @Action@, @Priority@, and @RuleId@
    --
    -- -   @Action@: Contains @Type@
    webACL :: Prelude.Maybe WebACL,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACL', 'getWebACLResponse_webACL' - Information about the WebACL that you specified in the @GetWebACL@
-- request. For more information, see the following topics:
--
-- -   WebACL: Contains @DefaultAction@, @MetricName@, @Name@, an array of
--     @Rule@ objects, and @WebACLId@
--
-- -   @DefaultAction@ (Data type is WafAction): Contains @Type@
--
-- -   @Rules@: Contains an array of @ActivatedRule@ objects, which contain
--     @Action@, @Priority@, and @RuleId@
--
-- -   @Action@: Contains @Type@
--
-- 'httpStatus', 'getWebACLResponse_httpStatus' - The response's http status code.
newGetWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWebACLResponse
newGetWebACLResponse pHttpStatus_ =
  GetWebACLResponse'
    { webACL = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the WebACL that you specified in the @GetWebACL@
-- request. For more information, see the following topics:
--
-- -   WebACL: Contains @DefaultAction@, @MetricName@, @Name@, an array of
--     @Rule@ objects, and @WebACLId@
--
-- -   @DefaultAction@ (Data type is WafAction): Contains @Type@
--
-- -   @Rules@: Contains an array of @ActivatedRule@ objects, which contain
--     @Action@, @Priority@, and @RuleId@
--
-- -   @Action@: Contains @Type@
getWebACLResponse_webACL :: Lens.Lens' GetWebACLResponse (Prelude.Maybe WebACL)
getWebACLResponse_webACL = Lens.lens (\GetWebACLResponse' {webACL} -> webACL) (\s@GetWebACLResponse' {} a -> s {webACL = a} :: GetWebACLResponse)

-- | The response's http status code.
getWebACLResponse_httpStatus :: Lens.Lens' GetWebACLResponse Prelude.Int
getWebACLResponse_httpStatus = Lens.lens (\GetWebACLResponse' {httpStatus} -> httpStatus) (\s@GetWebACLResponse' {} a -> s {httpStatus = a} :: GetWebACLResponse)

instance Prelude.NFData GetWebACLResponse where
  rnf GetWebACLResponse' {..} =
    Prelude.rnf webACL
      `Prelude.seq` Prelude.rnf httpStatus
