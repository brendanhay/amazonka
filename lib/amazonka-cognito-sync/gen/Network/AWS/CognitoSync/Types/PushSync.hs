{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.PushSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.PushSync
  ( PushSync (..),

    -- * Smart constructor
    mkPushSync,

    -- * Lenses
    psApplicationARNs,
    psRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration options to be applied to the identity pool.
--
-- /See:/ 'mkPushSync' smart constructor.
data PushSync = PushSync'
  { applicationARNs ::
      Lude.Maybe [Lude.Text],
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PushSync' with the minimum fields required to make a request.
--
-- * 'applicationARNs' - List of SNS platform application ARNs that could be used by clients.
-- * 'roleARN' - A role configured to allow Cognito to call SNS on behalf of the developer.
mkPushSync ::
  PushSync
mkPushSync =
  PushSync' {applicationARNs = Lude.Nothing, roleARN = Lude.Nothing}

-- | List of SNS platform application ARNs that could be used by clients.
--
-- /Note:/ Consider using 'applicationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psApplicationARNs :: Lens.Lens' PushSync (Lude.Maybe [Lude.Text])
psApplicationARNs = Lens.lens (applicationARNs :: PushSync -> Lude.Maybe [Lude.Text]) (\s a -> s {applicationARNs = a} :: PushSync)
{-# DEPRECATED psApplicationARNs "Use generic-lens or generic-optics with 'applicationARNs' instead." #-}

-- | A role configured to allow Cognito to call SNS on behalf of the developer.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psRoleARN :: Lens.Lens' PushSync (Lude.Maybe Lude.Text)
psRoleARN = Lens.lens (roleARN :: PushSync -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: PushSync)
{-# DEPRECATED psRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON PushSync where
  parseJSON =
    Lude.withObject
      "PushSync"
      ( \x ->
          PushSync'
            Lude.<$> (x Lude..:? "ApplicationArns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "RoleArn")
      )

instance Lude.ToJSON PushSync where
  toJSON PushSync' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ApplicationArns" Lude..=) Lude.<$> applicationARNs,
            ("RoleArn" Lude..=) Lude.<$> roleARN
          ]
      )
