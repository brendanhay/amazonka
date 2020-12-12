{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.InstanceInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.InstanceInfo
  ( InstanceInfo (..),

    -- * Smart constructor
    mkInstanceInfo,

    -- * Lenses
    iiRegisterTime,
    iiInstanceARN,
    iiDeregisterTime,
    iiIamUserARN,
    iiInstanceName,
    iiIamSessionARN,
    iiTags,
  )
where

import Network.AWS.CodeDeploy.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an on-premises instance.
--
-- /See:/ 'mkInstanceInfo' smart constructor.
data InstanceInfo = InstanceInfo'
  { registerTime ::
      Lude.Maybe Lude.Timestamp,
    instanceARN :: Lude.Maybe Lude.Text,
    deregisterTime :: Lude.Maybe Lude.Timestamp,
    iamUserARN :: Lude.Maybe Lude.Text,
    instanceName :: Lude.Maybe Lude.Text,
    iamSessionARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceInfo' with the minimum fields required to make a request.
--
-- * 'deregisterTime' - If the on-premises instance was deregistered, the time at which the on-premises instance was deregistered.
-- * 'iamSessionARN' - The ARN of the IAM session associated with the on-premises instance.
-- * 'iamUserARN' - The IAM user ARN associated with the on-premises instance.
-- * 'instanceARN' - The ARN of the on-premises instance.
-- * 'instanceName' - The name of the on-premises instance.
-- * 'registerTime' - The time at which the on-premises instance was registered.
-- * 'tags' - The tags currently associated with the on-premises instance.
mkInstanceInfo ::
  InstanceInfo
mkInstanceInfo =
  InstanceInfo'
    { registerTime = Lude.Nothing,
      instanceARN = Lude.Nothing,
      deregisterTime = Lude.Nothing,
      iamUserARN = Lude.Nothing,
      instanceName = Lude.Nothing,
      iamSessionARN = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The time at which the on-premises instance was registered.
--
-- /Note:/ Consider using 'registerTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiRegisterTime :: Lens.Lens' InstanceInfo (Lude.Maybe Lude.Timestamp)
iiRegisterTime = Lens.lens (registerTime :: InstanceInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {registerTime = a} :: InstanceInfo)
{-# DEPRECATED iiRegisterTime "Use generic-lens or generic-optics with 'registerTime' instead." #-}

-- | The ARN of the on-premises instance.
--
-- /Note:/ Consider using 'instanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiInstanceARN :: Lens.Lens' InstanceInfo (Lude.Maybe Lude.Text)
iiInstanceARN = Lens.lens (instanceARN :: InstanceInfo -> Lude.Maybe Lude.Text) (\s a -> s {instanceARN = a} :: InstanceInfo)
{-# DEPRECATED iiInstanceARN "Use generic-lens or generic-optics with 'instanceARN' instead." #-}

-- | If the on-premises instance was deregistered, the time at which the on-premises instance was deregistered.
--
-- /Note:/ Consider using 'deregisterTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiDeregisterTime :: Lens.Lens' InstanceInfo (Lude.Maybe Lude.Timestamp)
iiDeregisterTime = Lens.lens (deregisterTime :: InstanceInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {deregisterTime = a} :: InstanceInfo)
{-# DEPRECATED iiDeregisterTime "Use generic-lens or generic-optics with 'deregisterTime' instead." #-}

-- | The IAM user ARN associated with the on-premises instance.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIamUserARN :: Lens.Lens' InstanceInfo (Lude.Maybe Lude.Text)
iiIamUserARN = Lens.lens (iamUserARN :: InstanceInfo -> Lude.Maybe Lude.Text) (\s a -> s {iamUserARN = a} :: InstanceInfo)
{-# DEPRECATED iiIamUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | The name of the on-premises instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiInstanceName :: Lens.Lens' InstanceInfo (Lude.Maybe Lude.Text)
iiInstanceName = Lens.lens (instanceName :: InstanceInfo -> Lude.Maybe Lude.Text) (\s a -> s {instanceName = a} :: InstanceInfo)
{-# DEPRECATED iiInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

-- | The ARN of the IAM session associated with the on-premises instance.
--
-- /Note:/ Consider using 'iamSessionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiIamSessionARN :: Lens.Lens' InstanceInfo (Lude.Maybe Lude.Text)
iiIamSessionARN = Lens.lens (iamSessionARN :: InstanceInfo -> Lude.Maybe Lude.Text) (\s a -> s {iamSessionARN = a} :: InstanceInfo)
{-# DEPRECATED iiIamSessionARN "Use generic-lens or generic-optics with 'iamSessionARN' instead." #-}

-- | The tags currently associated with the on-premises instance.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iiTags :: Lens.Lens' InstanceInfo (Lude.Maybe [Tag])
iiTags = Lens.lens (tags :: InstanceInfo -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: InstanceInfo)
{-# DEPRECATED iiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON InstanceInfo where
  parseJSON =
    Lude.withObject
      "InstanceInfo"
      ( \x ->
          InstanceInfo'
            Lude.<$> (x Lude..:? "registerTime")
            Lude.<*> (x Lude..:? "instanceArn")
            Lude.<*> (x Lude..:? "deregisterTime")
            Lude.<*> (x Lude..:? "iamUserArn")
            Lude.<*> (x Lude..:? "instanceName")
            Lude.<*> (x Lude..:? "iamSessionArn")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
