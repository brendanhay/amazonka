{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryLimits
  ( DirectoryLimits (..),

    -- * Smart constructor
    mkDirectoryLimits,

    -- * Lenses
    dlConnectedDirectoriesCurrentCount,
    dlCloudOnlyMicrosoftADLimitReached,
    dlConnectedDirectoriesLimit,
    dlConnectedDirectoriesLimitReached,
    dlCloudOnlyMicrosoftADLimit,
    dlCloudOnlyDirectoriesLimit,
    dlCloudOnlyDirectoriesCurrentCount,
    dlCloudOnlyDirectoriesLimitReached,
    dlCloudOnlyMicrosoftADCurrentCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains directory limit information for a Region.
--
-- /See:/ 'mkDirectoryLimits' smart constructor.
data DirectoryLimits = DirectoryLimits'
  { -- | The current number of connected directories in the Region.
    connectedDirectoriesCurrentCount :: Lude.Maybe Lude.Natural,
    -- | Indicates if the AWS Managed Microsoft AD directory limit has been reached.
    cloudOnlyMicrosoftADLimitReached :: Lude.Maybe Lude.Bool,
    -- | The maximum number of connected directories allowed in the Region.
    connectedDirectoriesLimit :: Lude.Maybe Lude.Natural,
    -- | Indicates if the connected directory limit has been reached.
    connectedDirectoriesLimitReached :: Lude.Maybe Lude.Bool,
    -- | The maximum number of AWS Managed Microsoft AD directories allowed in the region.
    cloudOnlyMicrosoftADLimit :: Lude.Maybe Lude.Natural,
    -- | The maximum number of cloud directories allowed in the Region.
    cloudOnlyDirectoriesLimit :: Lude.Maybe Lude.Natural,
    -- | The current number of cloud directories in the Region.
    cloudOnlyDirectoriesCurrentCount :: Lude.Maybe Lude.Natural,
    -- | Indicates if the cloud directory limit has been reached.
    cloudOnlyDirectoriesLimitReached :: Lude.Maybe Lude.Bool,
    -- | The current number of AWS Managed Microsoft AD directories in the region.
    cloudOnlyMicrosoftADCurrentCount :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryLimits' with the minimum fields required to make a request.
--
-- * 'connectedDirectoriesCurrentCount' - The current number of connected directories in the Region.
-- * 'cloudOnlyMicrosoftADLimitReached' - Indicates if the AWS Managed Microsoft AD directory limit has been reached.
-- * 'connectedDirectoriesLimit' - The maximum number of connected directories allowed in the Region.
-- * 'connectedDirectoriesLimitReached' - Indicates if the connected directory limit has been reached.
-- * 'cloudOnlyMicrosoftADLimit' - The maximum number of AWS Managed Microsoft AD directories allowed in the region.
-- * 'cloudOnlyDirectoriesLimit' - The maximum number of cloud directories allowed in the Region.
-- * 'cloudOnlyDirectoriesCurrentCount' - The current number of cloud directories in the Region.
-- * 'cloudOnlyDirectoriesLimitReached' - Indicates if the cloud directory limit has been reached.
-- * 'cloudOnlyMicrosoftADCurrentCount' - The current number of AWS Managed Microsoft AD directories in the region.
mkDirectoryLimits ::
  DirectoryLimits
mkDirectoryLimits =
  DirectoryLimits'
    { connectedDirectoriesCurrentCount = Lude.Nothing,
      cloudOnlyMicrosoftADLimitReached = Lude.Nothing,
      connectedDirectoriesLimit = Lude.Nothing,
      connectedDirectoriesLimitReached = Lude.Nothing,
      cloudOnlyMicrosoftADLimit = Lude.Nothing,
      cloudOnlyDirectoriesLimit = Lude.Nothing,
      cloudOnlyDirectoriesCurrentCount = Lude.Nothing,
      cloudOnlyDirectoriesLimitReached = Lude.Nothing,
      cloudOnlyMicrosoftADCurrentCount = Lude.Nothing
    }

-- | The current number of connected directories in the Region.
--
-- /Note:/ Consider using 'connectedDirectoriesCurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlConnectedDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Natural)
dlConnectedDirectoriesCurrentCount = Lens.lens (connectedDirectoriesCurrentCount :: DirectoryLimits -> Lude.Maybe Lude.Natural) (\s a -> s {connectedDirectoriesCurrentCount = a} :: DirectoryLimits)
{-# DEPRECATED dlConnectedDirectoriesCurrentCount "Use generic-lens or generic-optics with 'connectedDirectoriesCurrentCount' instead." #-}

-- | Indicates if the AWS Managed Microsoft AD directory limit has been reached.
--
-- /Note:/ Consider using 'cloudOnlyMicrosoftADLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyMicrosoftADLimitReached :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Bool)
dlCloudOnlyMicrosoftADLimitReached = Lens.lens (cloudOnlyMicrosoftADLimitReached :: DirectoryLimits -> Lude.Maybe Lude.Bool) (\s a -> s {cloudOnlyMicrosoftADLimitReached = a} :: DirectoryLimits)
{-# DEPRECATED dlCloudOnlyMicrosoftADLimitReached "Use generic-lens or generic-optics with 'cloudOnlyMicrosoftADLimitReached' instead." #-}

-- | The maximum number of connected directories allowed in the Region.
--
-- /Note:/ Consider using 'connectedDirectoriesLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlConnectedDirectoriesLimit :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Natural)
dlConnectedDirectoriesLimit = Lens.lens (connectedDirectoriesLimit :: DirectoryLimits -> Lude.Maybe Lude.Natural) (\s a -> s {connectedDirectoriesLimit = a} :: DirectoryLimits)
{-# DEPRECATED dlConnectedDirectoriesLimit "Use generic-lens or generic-optics with 'connectedDirectoriesLimit' instead." #-}

-- | Indicates if the connected directory limit has been reached.
--
-- /Note:/ Consider using 'connectedDirectoriesLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlConnectedDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Bool)
dlConnectedDirectoriesLimitReached = Lens.lens (connectedDirectoriesLimitReached :: DirectoryLimits -> Lude.Maybe Lude.Bool) (\s a -> s {connectedDirectoriesLimitReached = a} :: DirectoryLimits)
{-# DEPRECATED dlConnectedDirectoriesLimitReached "Use generic-lens or generic-optics with 'connectedDirectoriesLimitReached' instead." #-}

-- | The maximum number of AWS Managed Microsoft AD directories allowed in the region.
--
-- /Note:/ Consider using 'cloudOnlyMicrosoftADLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyMicrosoftADLimit :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Natural)
dlCloudOnlyMicrosoftADLimit = Lens.lens (cloudOnlyMicrosoftADLimit :: DirectoryLimits -> Lude.Maybe Lude.Natural) (\s a -> s {cloudOnlyMicrosoftADLimit = a} :: DirectoryLimits)
{-# DEPRECATED dlCloudOnlyMicrosoftADLimit "Use generic-lens or generic-optics with 'cloudOnlyMicrosoftADLimit' instead." #-}

-- | The maximum number of cloud directories allowed in the Region.
--
-- /Note:/ Consider using 'cloudOnlyDirectoriesLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyDirectoriesLimit :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Natural)
dlCloudOnlyDirectoriesLimit = Lens.lens (cloudOnlyDirectoriesLimit :: DirectoryLimits -> Lude.Maybe Lude.Natural) (\s a -> s {cloudOnlyDirectoriesLimit = a} :: DirectoryLimits)
{-# DEPRECATED dlCloudOnlyDirectoriesLimit "Use generic-lens or generic-optics with 'cloudOnlyDirectoriesLimit' instead." #-}

-- | The current number of cloud directories in the Region.
--
-- /Note:/ Consider using 'cloudOnlyDirectoriesCurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyDirectoriesCurrentCount :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Natural)
dlCloudOnlyDirectoriesCurrentCount = Lens.lens (cloudOnlyDirectoriesCurrentCount :: DirectoryLimits -> Lude.Maybe Lude.Natural) (\s a -> s {cloudOnlyDirectoriesCurrentCount = a} :: DirectoryLimits)
{-# DEPRECATED dlCloudOnlyDirectoriesCurrentCount "Use generic-lens or generic-optics with 'cloudOnlyDirectoriesCurrentCount' instead." #-}

-- | Indicates if the cloud directory limit has been reached.
--
-- /Note:/ Consider using 'cloudOnlyDirectoriesLimitReached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyDirectoriesLimitReached :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Bool)
dlCloudOnlyDirectoriesLimitReached = Lens.lens (cloudOnlyDirectoriesLimitReached :: DirectoryLimits -> Lude.Maybe Lude.Bool) (\s a -> s {cloudOnlyDirectoriesLimitReached = a} :: DirectoryLimits)
{-# DEPRECATED dlCloudOnlyDirectoriesLimitReached "Use generic-lens or generic-optics with 'cloudOnlyDirectoriesLimitReached' instead." #-}

-- | The current number of AWS Managed Microsoft AD directories in the region.
--
-- /Note:/ Consider using 'cloudOnlyMicrosoftADCurrentCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlCloudOnlyMicrosoftADCurrentCount :: Lens.Lens' DirectoryLimits (Lude.Maybe Lude.Natural)
dlCloudOnlyMicrosoftADCurrentCount = Lens.lens (cloudOnlyMicrosoftADCurrentCount :: DirectoryLimits -> Lude.Maybe Lude.Natural) (\s a -> s {cloudOnlyMicrosoftADCurrentCount = a} :: DirectoryLimits)
{-# DEPRECATED dlCloudOnlyMicrosoftADCurrentCount "Use generic-lens or generic-optics with 'cloudOnlyMicrosoftADCurrentCount' instead." #-}

instance Lude.FromJSON DirectoryLimits where
  parseJSON =
    Lude.withObject
      "DirectoryLimits"
      ( \x ->
          DirectoryLimits'
            Lude.<$> (x Lude..:? "ConnectedDirectoriesCurrentCount")
            Lude.<*> (x Lude..:? "CloudOnlyMicrosoftADLimitReached")
            Lude.<*> (x Lude..:? "ConnectedDirectoriesLimit")
            Lude.<*> (x Lude..:? "ConnectedDirectoriesLimitReached")
            Lude.<*> (x Lude..:? "CloudOnlyMicrosoftADLimit")
            Lude.<*> (x Lude..:? "CloudOnlyDirectoriesLimit")
            Lude.<*> (x Lude..:? "CloudOnlyDirectoriesCurrentCount")
            Lude.<*> (x Lude..:? "CloudOnlyDirectoriesLimitReached")
            Lude.<*> (x Lude..:? "CloudOnlyMicrosoftADCurrentCount")
      )
