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
-- Module      : Network.AWS.WAFRegional.GetIPSet
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
-- Returns the IPSet that is specified by @IPSetId@.
module Network.AWS.WAFRegional.GetIPSet
  ( -- * Creating a Request
    GetIPSet (..),
    newGetIPSet,

    -- * Request Lenses
    getIPSet_iPSetId,

    -- * Destructuring the Response
    GetIPSetResponse (..),
    newGetIPSetResponse,

    -- * Response Lenses
    getIPSetResponse_iPSet,
    getIPSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newGetIPSet' smart constructor.
data GetIPSet = GetIPSet'
  { -- | The @IPSetId@ of the IPSet that you want to get. @IPSetId@ is returned
    -- by CreateIPSet and by ListIPSets.
    iPSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSetId', 'getIPSet_iPSetId' - The @IPSetId@ of the IPSet that you want to get. @IPSetId@ is returned
-- by CreateIPSet and by ListIPSets.
newGetIPSet ::
  -- | 'iPSetId'
  Prelude.Text ->
  GetIPSet
newGetIPSet pIPSetId_ =
  GetIPSet' {iPSetId = pIPSetId_}

-- | The @IPSetId@ of the IPSet that you want to get. @IPSetId@ is returned
-- by CreateIPSet and by ListIPSets.
getIPSet_iPSetId :: Lens.Lens' GetIPSet Prelude.Text
getIPSet_iPSetId = Lens.lens (\GetIPSet' {iPSetId} -> iPSetId) (\s@GetIPSet' {} a -> s {iPSetId = a} :: GetIPSet)

instance Core.AWSRequest GetIPSet where
  type AWSResponse GetIPSet = GetIPSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIPSetResponse'
            Prelude.<$> (x Core..?> "IPSet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIPSet

instance Prelude.NFData GetIPSet

instance Core.ToHeaders GetIPSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.GetIPSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetIPSet where
  toJSON GetIPSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("IPSetId" Core..= iPSetId)]
      )

instance Core.ToPath GetIPSet where
  toPath = Prelude.const "/"

instance Core.ToQuery GetIPSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIPSetResponse' smart constructor.
data GetIPSetResponse = GetIPSetResponse'
  { -- | Information about the IPSet that you specified in the @GetIPSet@
    -- request. For more information, see the following topics:
    --
    -- -   IPSet: Contains @IPSetDescriptors@, @IPSetId@, and @Name@
    --
    -- -   @IPSetDescriptors@: Contains an array of IPSetDescriptor objects.
    --     Each @IPSetDescriptor@ object contains @Type@ and @Value@
    iPSet :: Prelude.Maybe IPSet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIPSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSet', 'getIPSetResponse_iPSet' - Information about the IPSet that you specified in the @GetIPSet@
-- request. For more information, see the following topics:
--
-- -   IPSet: Contains @IPSetDescriptors@, @IPSetId@, and @Name@
--
-- -   @IPSetDescriptors@: Contains an array of IPSetDescriptor objects.
--     Each @IPSetDescriptor@ object contains @Type@ and @Value@
--
-- 'httpStatus', 'getIPSetResponse_httpStatus' - The response's http status code.
newGetIPSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIPSetResponse
newGetIPSetResponse pHttpStatus_ =
  GetIPSetResponse'
    { iPSet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the IPSet that you specified in the @GetIPSet@
-- request. For more information, see the following topics:
--
-- -   IPSet: Contains @IPSetDescriptors@, @IPSetId@, and @Name@
--
-- -   @IPSetDescriptors@: Contains an array of IPSetDescriptor objects.
--     Each @IPSetDescriptor@ object contains @Type@ and @Value@
getIPSetResponse_iPSet :: Lens.Lens' GetIPSetResponse (Prelude.Maybe IPSet)
getIPSetResponse_iPSet = Lens.lens (\GetIPSetResponse' {iPSet} -> iPSet) (\s@GetIPSetResponse' {} a -> s {iPSet = a} :: GetIPSetResponse)

-- | The response's http status code.
getIPSetResponse_httpStatus :: Lens.Lens' GetIPSetResponse Prelude.Int
getIPSetResponse_httpStatus = Lens.lens (\GetIPSetResponse' {httpStatus} -> httpStatus) (\s@GetIPSetResponse' {} a -> s {httpStatus = a} :: GetIPSetResponse)

instance Prelude.NFData GetIPSetResponse
