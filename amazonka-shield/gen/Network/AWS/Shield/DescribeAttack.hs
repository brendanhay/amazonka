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
-- Module      : Network.AWS.Shield.DescribeAttack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a DDoS attack.
module Network.AWS.Shield.DescribeAttack
  ( -- * Creating a Request
    DescribeAttack (..),
    newDescribeAttack,

    -- * Request Lenses
    describeAttack_attackId,

    -- * Destructuring the Response
    DescribeAttackResponse (..),
    newDescribeAttackResponse,

    -- * Response Lenses
    describeAttackResponse_attack,
    describeAttackResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDescribeAttack' smart constructor.
data DescribeAttack = DescribeAttack'
  { -- | The unique identifier (ID) for the attack that to be described.
    attackId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAttack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attackId', 'describeAttack_attackId' - The unique identifier (ID) for the attack that to be described.
newDescribeAttack ::
  -- | 'attackId'
  Prelude.Text ->
  DescribeAttack
newDescribeAttack pAttackId_ =
  DescribeAttack' {attackId = pAttackId_}

-- | The unique identifier (ID) for the attack that to be described.
describeAttack_attackId :: Lens.Lens' DescribeAttack Prelude.Text
describeAttack_attackId = Lens.lens (\DescribeAttack' {attackId} -> attackId) (\s@DescribeAttack' {} a -> s {attackId = a} :: DescribeAttack)

instance Core.AWSRequest DescribeAttack where
  type
    AWSResponse DescribeAttack =
      DescribeAttackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAttackResponse'
            Prelude.<$> (x Core..?> "Attack")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAttack

instance Prelude.NFData DescribeAttack

instance Core.ToHeaders DescribeAttack where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DescribeAttack" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAttack where
  toJSON DescribeAttack' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("AttackId" Core..= attackId)]
      )

instance Core.ToPath DescribeAttack where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAttack where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAttackResponse' smart constructor.
data DescribeAttackResponse = DescribeAttackResponse'
  { -- | The attack that is described.
    attack :: Prelude.Maybe AttackDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAttackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attack', 'describeAttackResponse_attack' - The attack that is described.
--
-- 'httpStatus', 'describeAttackResponse_httpStatus' - The response's http status code.
newDescribeAttackResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAttackResponse
newDescribeAttackResponse pHttpStatus_ =
  DescribeAttackResponse'
    { attack = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attack that is described.
describeAttackResponse_attack :: Lens.Lens' DescribeAttackResponse (Prelude.Maybe AttackDetail)
describeAttackResponse_attack = Lens.lens (\DescribeAttackResponse' {attack} -> attack) (\s@DescribeAttackResponse' {} a -> s {attack = a} :: DescribeAttackResponse)

-- | The response's http status code.
describeAttackResponse_httpStatus :: Lens.Lens' DescribeAttackResponse Prelude.Int
describeAttackResponse_httpStatus = Lens.lens (\DescribeAttackResponse' {httpStatus} -> httpStatus) (\s@DescribeAttackResponse' {} a -> s {httpStatus = a} :: DescribeAttackResponse)

instance Prelude.NFData DescribeAttackResponse
