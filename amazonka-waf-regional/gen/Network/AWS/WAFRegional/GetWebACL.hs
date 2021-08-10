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
-- Module      : Network.AWS.WAFRegional.GetWebACL
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
-- Returns the WebACL that is specified by @WebACLId@.
module Network.AWS.WAFRegional.GetWebACL
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWebACLResponse'
            Prelude.<$> (x Core..?> "WebACL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWebACL

instance Prelude.NFData GetWebACL

instance Core.ToHeaders GetWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.GetWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetWebACL where
  toJSON GetWebACL' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("WebACLId" Core..= webACLId)]
      )

instance Core.ToPath GetWebACL where
  toPath = Prelude.const "/"

instance Core.ToQuery GetWebACL where
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

instance Prelude.NFData GetWebACLResponse
