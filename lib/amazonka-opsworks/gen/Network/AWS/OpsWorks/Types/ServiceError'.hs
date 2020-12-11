-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ServiceError'
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ServiceError'
  ( ServiceError' (..),

    -- * Smart constructor
    mkServiceError',

    -- * Lenses
    seInstanceId,
    seCreatedAt,
    seServiceErrorId,
    seType,
    seStackId,
    seMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an AWS OpsWorks Stacks service error.
--
-- /See:/ 'mkServiceError'' smart constructor.
data ServiceError' = ServiceError''
  { instanceId ::
      Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Text,
    serviceErrorId :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceError'' with the minimum fields required to make a request.
--
-- * 'createdAt' - When the error occurred.
-- * 'instanceId' - The instance ID.
-- * 'message' - A message that describes the error.
-- * 'serviceErrorId' - The error ID.
-- * 'stackId' - The stack ID.
-- * 'type'' - The error type.
mkServiceError' ::
  ServiceError'
mkServiceError' =
  ServiceError''
    { instanceId = Lude.Nothing,
      createdAt = Lude.Nothing,
      serviceErrorId = Lude.Nothing,
      type' = Lude.Nothing,
      stackId = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seInstanceId :: Lens.Lens' ServiceError' (Lude.Maybe Lude.Text)
seInstanceId = Lens.lens (instanceId :: ServiceError' -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ServiceError')
{-# DEPRECATED seInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | When the error occurred.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCreatedAt :: Lens.Lens' ServiceError' (Lude.Maybe Lude.Text)
seCreatedAt = Lens.lens (createdAt :: ServiceError' -> Lude.Maybe Lude.Text) (\s a -> s {createdAt = a} :: ServiceError')
{-# DEPRECATED seCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The error ID.
--
-- /Note:/ Consider using 'serviceErrorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seServiceErrorId :: Lens.Lens' ServiceError' (Lude.Maybe Lude.Text)
seServiceErrorId = Lens.lens (serviceErrorId :: ServiceError' -> Lude.Maybe Lude.Text) (\s a -> s {serviceErrorId = a} :: ServiceError')
{-# DEPRECATED seServiceErrorId "Use generic-lens or generic-optics with 'serviceErrorId' instead." #-}

-- | The error type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seType :: Lens.Lens' ServiceError' (Lude.Maybe Lude.Text)
seType = Lens.lens (type' :: ServiceError' -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ServiceError')
{-# DEPRECATED seType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStackId :: Lens.Lens' ServiceError' (Lude.Maybe Lude.Text)
seStackId = Lens.lens (stackId :: ServiceError' -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: ServiceError')
{-# DEPRECATED seStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | A message that describes the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' ServiceError' (Lude.Maybe Lude.Text)
seMessage = Lens.lens (message :: ServiceError' -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ServiceError')
{-# DEPRECATED seMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ServiceError' where
  parseJSON =
    Lude.withObject
      "ServiceError'"
      ( \x ->
          ServiceError''
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "CreatedAt")
            Lude.<*> (x Lude..:? "ServiceErrorId")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "Message")
      )
