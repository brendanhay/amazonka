-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportInstanceTaskDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportInstanceTaskDetails
  ( ImportInstanceTaskDetails (..),

    -- * Smart constructor
    mkImportInstanceTaskDetails,

    -- * Lenses
    iitdInstanceId,
    iitdPlatform,
    iitdVolumes,
    iitdDescription,
  )
where

import Network.AWS.EC2.Types.ImportInstanceVolumeDetailItem
import Network.AWS.EC2.Types.PlatformValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an import instance task.
--
-- /See:/ 'mkImportInstanceTaskDetails' smart constructor.
data ImportInstanceTaskDetails = ImportInstanceTaskDetails'
  { instanceId ::
      Lude.Maybe Lude.Text,
    platform :: Lude.Maybe PlatformValues,
    volumes ::
      Lude.Maybe
        [ImportInstanceVolumeDetailItem],
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportInstanceTaskDetails' with the minimum fields required to make a request.
--
-- * 'description' - A description of the task.
-- * 'instanceId' - The ID of the instance.
-- * 'platform' - The instance operating system.
-- * 'volumes' - The volumes.
mkImportInstanceTaskDetails ::
  ImportInstanceTaskDetails
mkImportInstanceTaskDetails =
  ImportInstanceTaskDetails'
    { instanceId = Lude.Nothing,
      platform = Lude.Nothing,
      volumes = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitdInstanceId :: Lens.Lens' ImportInstanceTaskDetails (Lude.Maybe Lude.Text)
iitdInstanceId = Lens.lens (instanceId :: ImportInstanceTaskDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ImportInstanceTaskDetails)
{-# DEPRECATED iitdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The instance operating system.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitdPlatform :: Lens.Lens' ImportInstanceTaskDetails (Lude.Maybe PlatformValues)
iitdPlatform = Lens.lens (platform :: ImportInstanceTaskDetails -> Lude.Maybe PlatformValues) (\s a -> s {platform = a} :: ImportInstanceTaskDetails)
{-# DEPRECATED iitdPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The volumes.
--
-- /Note:/ Consider using 'volumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitdVolumes :: Lens.Lens' ImportInstanceTaskDetails (Lude.Maybe [ImportInstanceVolumeDetailItem])
iitdVolumes = Lens.lens (volumes :: ImportInstanceTaskDetails -> Lude.Maybe [ImportInstanceVolumeDetailItem]) (\s a -> s {volumes = a} :: ImportInstanceTaskDetails)
{-# DEPRECATED iitdVolumes "Use generic-lens or generic-optics with 'volumes' instead." #-}

-- | A description of the task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iitdDescription :: Lens.Lens' ImportInstanceTaskDetails (Lude.Maybe Lude.Text)
iitdDescription = Lens.lens (description :: ImportInstanceTaskDetails -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImportInstanceTaskDetails)
{-# DEPRECATED iitdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ImportInstanceTaskDetails where
  parseXML x =
    ImportInstanceTaskDetails'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "platform")
      Lude.<*> ( x Lude..@? "volumes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "description")
