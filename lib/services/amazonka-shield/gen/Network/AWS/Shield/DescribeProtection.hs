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
-- Module      : Network.AWS.Shield.DescribeProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of a Protection object.
module Network.AWS.Shield.DescribeProtection
  ( -- * Creating a Request
    DescribeProtection (..),
    newDescribeProtection,

    -- * Request Lenses
    describeProtection_protectionId,
    describeProtection_resourceArn,

    -- * Destructuring the Response
    DescribeProtectionResponse (..),
    newDescribeProtectionResponse,

    -- * Response Lenses
    describeProtectionResponse_protection,
    describeProtectionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDescribeProtection' smart constructor.
data DescribeProtection = DescribeProtection'
  { -- | The unique identifier (ID) for the Protection object that is described.
    -- When submitting the @DescribeProtection@ request you must provide either
    -- the @ResourceArn@ or the @ProtectionID@, but not both.
    protectionId :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the Amazon Web Services resource for
    -- the Protection object that is described. When submitting the
    -- @DescribeProtection@ request you must provide either the @ResourceArn@
    -- or the @ProtectionID@, but not both.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionId', 'describeProtection_protectionId' - The unique identifier (ID) for the Protection object that is described.
-- When submitting the @DescribeProtection@ request you must provide either
-- the @ResourceArn@ or the @ProtectionID@, but not both.
--
-- 'resourceArn', 'describeProtection_resourceArn' - The ARN (Amazon Resource Name) of the Amazon Web Services resource for
-- the Protection object that is described. When submitting the
-- @DescribeProtection@ request you must provide either the @ResourceArn@
-- or the @ProtectionID@, but not both.
newDescribeProtection ::
  DescribeProtection
newDescribeProtection =
  DescribeProtection'
    { protectionId = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The unique identifier (ID) for the Protection object that is described.
-- When submitting the @DescribeProtection@ request you must provide either
-- the @ResourceArn@ or the @ProtectionID@, but not both.
describeProtection_protectionId :: Lens.Lens' DescribeProtection (Prelude.Maybe Prelude.Text)
describeProtection_protectionId = Lens.lens (\DescribeProtection' {protectionId} -> protectionId) (\s@DescribeProtection' {} a -> s {protectionId = a} :: DescribeProtection)

-- | The ARN (Amazon Resource Name) of the Amazon Web Services resource for
-- the Protection object that is described. When submitting the
-- @DescribeProtection@ request you must provide either the @ResourceArn@
-- or the @ProtectionID@, but not both.
describeProtection_resourceArn :: Lens.Lens' DescribeProtection (Prelude.Maybe Prelude.Text)
describeProtection_resourceArn = Lens.lens (\DescribeProtection' {resourceArn} -> resourceArn) (\s@DescribeProtection' {} a -> s {resourceArn = a} :: DescribeProtection)

instance Core.AWSRequest DescribeProtection where
  type
    AWSResponse DescribeProtection =
      DescribeProtectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProtectionResponse'
            Prelude.<$> (x Core..?> "Protection")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProtection

instance Prelude.NFData DescribeProtection

instance Core.ToHeaders DescribeProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DescribeProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProtection where
  toJSON DescribeProtection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProtectionId" Core..=) Prelude.<$> protectionId,
            ("ResourceArn" Core..=) Prelude.<$> resourceArn
          ]
      )

instance Core.ToPath DescribeProtection where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProtectionResponse' smart constructor.
data DescribeProtectionResponse = DescribeProtectionResponse'
  { -- | The Protection object that is described.
    protection :: Prelude.Maybe Protection,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protection', 'describeProtectionResponse_protection' - The Protection object that is described.
--
-- 'httpStatus', 'describeProtectionResponse_httpStatus' - The response's http status code.
newDescribeProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProtectionResponse
newDescribeProtectionResponse pHttpStatus_ =
  DescribeProtectionResponse'
    { protection =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Protection object that is described.
describeProtectionResponse_protection :: Lens.Lens' DescribeProtectionResponse (Prelude.Maybe Protection)
describeProtectionResponse_protection = Lens.lens (\DescribeProtectionResponse' {protection} -> protection) (\s@DescribeProtectionResponse' {} a -> s {protection = a} :: DescribeProtectionResponse)

-- | The response's http status code.
describeProtectionResponse_httpStatus :: Lens.Lens' DescribeProtectionResponse Prelude.Int
describeProtectionResponse_httpStatus = Lens.lens (\DescribeProtectionResponse' {httpStatus} -> httpStatus) (\s@DescribeProtectionResponse' {} a -> s {httpStatus = a} :: DescribeProtectionResponse)

instance Prelude.NFData DescribeProtectionResponse
