{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeAttack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a DDoS attack.
module Network.AWS.Shield.DescribeAttack
  ( -- * Creating a request
    DescribeAttack (..),
    mkDescribeAttack,

    -- ** Request lenses
    daAttackId,

    -- * Destructuring the response
    DescribeAttackResponse (..),
    mkDescribeAttackResponse,

    -- ** Response lenses
    darsAttack,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDescribeAttack' smart constructor.
newtype DescribeAttack = DescribeAttack'
  { -- | The unique identifier (ID) for the attack that to be described.
    attackId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAttack' with the minimum fields required to make a request.
--
-- * 'attackId' - The unique identifier (ID) for the attack that to be described.
mkDescribeAttack ::
  -- | 'attackId'
  Lude.Text ->
  DescribeAttack
mkDescribeAttack pAttackId_ =
  DescribeAttack' {attackId = pAttackId_}

-- | The unique identifier (ID) for the attack that to be described.
--
-- /Note:/ Consider using 'attackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAttackId :: Lens.Lens' DescribeAttack Lude.Text
daAttackId = Lens.lens (attackId :: DescribeAttack -> Lude.Text) (\s a -> s {attackId = a} :: DescribeAttack)
{-# DEPRECATED daAttackId "Use generic-lens or generic-optics with 'attackId' instead." #-}

instance Lude.AWSRequest DescribeAttack where
  type Rs DescribeAttack = DescribeAttackResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAttackResponse'
            Lude.<$> (x Lude..?> "Attack") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAttack where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DescribeAttack" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAttack where
  toJSON DescribeAttack' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("AttackId" Lude..= attackId)])

instance Lude.ToPath DescribeAttack where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAttack where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAttackResponse' smart constructor.
data DescribeAttackResponse = DescribeAttackResponse'
  { -- | The attack that is described.
    attack :: Lude.Maybe AttackDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAttackResponse' with the minimum fields required to make a request.
--
-- * 'attack' - The attack that is described.
-- * 'responseStatus' - The response status code.
mkDescribeAttackResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAttackResponse
mkDescribeAttackResponse pResponseStatus_ =
  DescribeAttackResponse'
    { attack = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The attack that is described.
--
-- /Note:/ Consider using 'attack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsAttack :: Lens.Lens' DescribeAttackResponse (Lude.Maybe AttackDetail)
darsAttack = Lens.lens (attack :: DescribeAttackResponse -> Lude.Maybe AttackDetail) (\s a -> s {attack = a} :: DescribeAttackResponse)
{-# DEPRECATED darsAttack "Use generic-lens or generic-optics with 'attack' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DescribeAttackResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DescribeAttackResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAttackResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
