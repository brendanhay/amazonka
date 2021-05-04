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
-- Module      : Network.AWS.Organizations.DescribeHandshake
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a previously requested handshake. The
-- handshake ID comes from the response to the original
-- InviteAccountToOrganization operation that generated the handshake.
--
-- You can access handshakes that are @ACCEPTED@, @DECLINED@, or @CANCELED@
-- for only 30 days after they change to that state. They\'re then deleted
-- and no longer accessible.
--
-- This operation can be called from any account in the organization.
module Network.AWS.Organizations.DescribeHandshake
  ( -- * Creating a Request
    DescribeHandshake (..),
    newDescribeHandshake,

    -- * Request Lenses
    describeHandshake_handshakeId,

    -- * Destructuring the Response
    DescribeHandshakeResponse (..),
    newDescribeHandshakeResponse,

    -- * Response Lenses
    describeHandshakeResponse_handshake,
    describeHandshakeResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeHandshake' smart constructor.
data DescribeHandshake = DescribeHandshake'
  { -- | The unique identifier (ID) of the handshake that you want information
    -- about. You can get the ID from the original call to
    -- InviteAccountToOrganization, or from a call to ListHandshakesForAccount
    -- or ListHandshakesForOrganization.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
    -- string requires \"h-\" followed by from 8 to 32 lowercase letters or
    -- digits.
    handshakeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeHandshake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshakeId', 'describeHandshake_handshakeId' - The unique identifier (ID) of the handshake that you want information
-- about. You can get the ID from the original call to
-- InviteAccountToOrganization, or from a call to ListHandshakesForAccount
-- or ListHandshakesForOrganization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
newDescribeHandshake ::
  -- | 'handshakeId'
  Prelude.Text ->
  DescribeHandshake
newDescribeHandshake pHandshakeId_ =
  DescribeHandshake' {handshakeId = pHandshakeId_}

-- | The unique identifier (ID) of the handshake that you want information
-- about. You can get the ID from the original call to
-- InviteAccountToOrganization, or from a call to ListHandshakesForAccount
-- or ListHandshakesForOrganization.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID
-- string requires \"h-\" followed by from 8 to 32 lowercase letters or
-- digits.
describeHandshake_handshakeId :: Lens.Lens' DescribeHandshake Prelude.Text
describeHandshake_handshakeId = Lens.lens (\DescribeHandshake' {handshakeId} -> handshakeId) (\s@DescribeHandshake' {} a -> s {handshakeId = a} :: DescribeHandshake)

instance Prelude.AWSRequest DescribeHandshake where
  type Rs DescribeHandshake = DescribeHandshakeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeHandshakeResponse'
            Prelude.<$> (x Prelude..?> "Handshake")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeHandshake

instance Prelude.NFData DescribeHandshake

instance Prelude.ToHeaders DescribeHandshake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSOrganizationsV20161128.DescribeHandshake" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeHandshake where
  toJSON DescribeHandshake' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("HandshakeId" Prelude..= handshakeId)
          ]
      )

instance Prelude.ToPath DescribeHandshake where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeHandshake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeHandshakeResponse' smart constructor.
data DescribeHandshakeResponse = DescribeHandshakeResponse'
  { -- | A structure that contains information about the specified handshake.
    handshake :: Prelude.Maybe Handshake,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeHandshakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'handshake', 'describeHandshakeResponse_handshake' - A structure that contains information about the specified handshake.
--
-- 'httpStatus', 'describeHandshakeResponse_httpStatus' - The response's http status code.
newDescribeHandshakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeHandshakeResponse
newDescribeHandshakeResponse pHttpStatus_ =
  DescribeHandshakeResponse'
    { handshake =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains information about the specified handshake.
describeHandshakeResponse_handshake :: Lens.Lens' DescribeHandshakeResponse (Prelude.Maybe Handshake)
describeHandshakeResponse_handshake = Lens.lens (\DescribeHandshakeResponse' {handshake} -> handshake) (\s@DescribeHandshakeResponse' {} a -> s {handshake = a} :: DescribeHandshakeResponse)

-- | The response's http status code.
describeHandshakeResponse_httpStatus :: Lens.Lens' DescribeHandshakeResponse Prelude.Int
describeHandshakeResponse_httpStatus = Lens.lens (\DescribeHandshakeResponse' {httpStatus} -> httpStatus) (\s@DescribeHandshakeResponse' {} a -> s {httpStatus = a} :: DescribeHandshakeResponse)

instance Prelude.NFData DescribeHandshakeResponse
