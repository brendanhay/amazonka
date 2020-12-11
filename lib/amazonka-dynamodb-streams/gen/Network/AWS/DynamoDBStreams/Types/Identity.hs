-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.Identity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.Identity
  ( Identity (..),

    -- * Smart constructor
    mkIdentity,

    -- * Lenses
    iPrincipalId,
    iType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the type of identity that made the request.
--
-- /See:/ 'mkIdentity' smart constructor.
data Identity = Identity'
  { principalId :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Identity' with the minimum fields required to make a request.
--
-- * 'principalId' - A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
-- * 'type'' - The type of the identity. For Time To Live, the type is "Service".
mkIdentity ::
  Identity
mkIdentity =
  Identity' {principalId = Lude.Nothing, type' = Lude.Nothing}

-- | A unique identifier for the entity that made the call. For Time To Live, the principalId is "dynamodb.amazonaws.com".
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrincipalId :: Lens.Lens' Identity (Lude.Maybe Lude.Text)
iPrincipalId = Lens.lens (principalId :: Identity -> Lude.Maybe Lude.Text) (\s a -> s {principalId = a} :: Identity)
{-# DEPRECATED iPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The type of the identity. For Time To Live, the type is "Service".
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iType :: Lens.Lens' Identity (Lude.Maybe Lude.Text)
iType = Lens.lens (type' :: Identity -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Identity)
{-# DEPRECATED iType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Identity where
  parseJSON =
    Lude.withObject
      "Identity"
      ( \x ->
          Identity'
            Lude.<$> (x Lude..:? "PrincipalId") Lude.<*> (x Lude..:? "Type")
      )
