{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53.GetCheckerIpRanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Route 53 does not perform authorization for this API because it
-- retrieves information that is already available to the public.
--
-- @GetCheckerIpRanges@ still works, but we recommend that you download
-- ip-ranges.json, which includes IP address ranges for all AWS services.
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/route-53-ip-addresses.html IP Address Ranges of Amazon Route 53 Servers>
-- in the /Amazon Route 53 Developer Guide/.
module Network.AWS.Route53.GetCheckerIpRanges
  ( -- * Creating a Request
    GetCheckerIpRanges (..),
    newGetCheckerIpRanges,

    -- * Destructuring the Response
    GetCheckerIpRangesResponse (..),
    newGetCheckerIpRangesResponse,

    -- * Response Lenses
    getCheckerIpRangesResponse_httpStatus,
    getCheckerIpRangesResponse_checkerIpRanges,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | Empty request.
--
-- /See:/ 'newGetCheckerIpRanges' smart constructor.
data GetCheckerIpRanges = GetCheckerIpRanges'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCheckerIpRanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetCheckerIpRanges ::
  GetCheckerIpRanges
newGetCheckerIpRanges = GetCheckerIpRanges'

instance Prelude.AWSRequest GetCheckerIpRanges where
  type
    Rs GetCheckerIpRanges =
      GetCheckerIpRangesResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetCheckerIpRangesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..@? "CheckerIpRanges"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.parseXMLList "member"
                        )
      )

instance Prelude.Hashable GetCheckerIpRanges

instance Prelude.NFData GetCheckerIpRanges

instance Prelude.ToHeaders GetCheckerIpRanges where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetCheckerIpRanges where
  toPath = Prelude.const "/2013-04-01/checkeripranges"

instance Prelude.ToQuery GetCheckerIpRanges where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the @CheckerIpRanges@ element.
--
-- /See:/ 'newGetCheckerIpRangesResponse' smart constructor.
data GetCheckerIpRangesResponse = GetCheckerIpRangesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains sorted list of IP ranges in CIDR format for
    -- Amazon Route 53 health checkers.
    checkerIpRanges :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCheckerIpRangesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCheckerIpRangesResponse_httpStatus' - The response's http status code.
--
-- 'checkerIpRanges', 'getCheckerIpRangesResponse_checkerIpRanges' - A complex type that contains sorted list of IP ranges in CIDR format for
-- Amazon Route 53 health checkers.
newGetCheckerIpRangesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCheckerIpRangesResponse
newGetCheckerIpRangesResponse pHttpStatus_ =
  GetCheckerIpRangesResponse'
    { httpStatus =
        pHttpStatus_,
      checkerIpRanges = Prelude.mempty
    }

-- | The response's http status code.
getCheckerIpRangesResponse_httpStatus :: Lens.Lens' GetCheckerIpRangesResponse Prelude.Int
getCheckerIpRangesResponse_httpStatus = Lens.lens (\GetCheckerIpRangesResponse' {httpStatus} -> httpStatus) (\s@GetCheckerIpRangesResponse' {} a -> s {httpStatus = a} :: GetCheckerIpRangesResponse)

-- | A complex type that contains sorted list of IP ranges in CIDR format for
-- Amazon Route 53 health checkers.
getCheckerIpRangesResponse_checkerIpRanges :: Lens.Lens' GetCheckerIpRangesResponse [Prelude.Text]
getCheckerIpRangesResponse_checkerIpRanges = Lens.lens (\GetCheckerIpRangesResponse' {checkerIpRanges} -> checkerIpRanges) (\s@GetCheckerIpRangesResponse' {} a -> s {checkerIpRanges = a} :: GetCheckerIpRangesResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData GetCheckerIpRangesResponse
