-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Delegate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Delegate
  ( Delegate (..),

    -- * Smart constructor
    mkDelegate,

    -- * Lenses
    dId,
    dType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkMail.Types.MemberType

-- | The name of the attribute, which is one of the values defined in the UserAttribute enumeration.
--
-- /See:/ 'mkDelegate' smart constructor.
data Delegate = Delegate' {id :: Lude.Text, type' :: MemberType}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Delegate' with the minimum fields required to make a request.
--
-- * 'id' - The identifier for the user or group associated as the resource's delegate.
-- * 'type'' - The type of the delegate: user or group.
mkDelegate ::
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  MemberType ->
  Delegate
mkDelegate pId_ pType_ = Delegate' {id = pId_, type' = pType_}

-- | The identifier for the user or group associated as the resource's delegate.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dId :: Lens.Lens' Delegate Lude.Text
dId = Lens.lens (id :: Delegate -> Lude.Text) (\s a -> s {id = a} :: Delegate)
{-# DEPRECATED dId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the delegate: user or group.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dType :: Lens.Lens' Delegate MemberType
dType = Lens.lens (type' :: Delegate -> MemberType) (\s a -> s {type' = a} :: Delegate)
{-# DEPRECATED dType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Delegate where
  parseJSON =
    Lude.withObject
      "Delegate"
      ( \x ->
          Delegate' Lude.<$> (x Lude..: "Id") Lude.<*> (x Lude..: "Type")
      )
