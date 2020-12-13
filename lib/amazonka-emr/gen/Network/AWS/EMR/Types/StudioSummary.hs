{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StudioSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StudioSummary
  ( StudioSummary (..),

    -- * Smart constructor
    mkStudioSummary,

    -- * Lenses
    ssCreationTime,
    ssStudioId,
    ssVPCId,
    ssURL,
    ssName,
    ssDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details for an Amazon EMR Studio, including ID, Name, VPC, and Description. The details do not include subnets, IAM roles, security groups, or tags associated with the Studio.
--
-- /See:/ 'mkStudioSummary' smart constructor.
data StudioSummary = StudioSummary'
  { -- | The time when the Amazon EMR Studio was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the Amazon EMR Studio.
    studioId :: Lude.Maybe Lude.Text,
    -- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the Amazon EMR Studio.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The unique access URL of the Amazon EMR Studio.
    url :: Lude.Maybe Lude.Text,
    -- | The name of the Amazon EMR Studio.
    name :: Lude.Maybe Lude.Text,
    -- | The detailed description of the EMR Studio.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StudioSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time when the Amazon EMR Studio was created.
-- * 'studioId' - The ID of the Amazon EMR Studio.
-- * 'vpcId' - The ID of the Virtual Private Cloud (Amazon VPC) associated with the Amazon EMR Studio.
-- * 'url' - The unique access URL of the Amazon EMR Studio.
-- * 'name' - The name of the Amazon EMR Studio.
-- * 'description' - The detailed description of the EMR Studio.
mkStudioSummary ::
  StudioSummary
mkStudioSummary =
  StudioSummary'
    { creationTime = Lude.Nothing,
      studioId = Lude.Nothing,
      vpcId = Lude.Nothing,
      url = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The time when the Amazon EMR Studio was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCreationTime :: Lens.Lens' StudioSummary (Lude.Maybe Lude.Timestamp)
ssCreationTime = Lens.lens (creationTime :: StudioSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: StudioSummary)
{-# DEPRECATED ssCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStudioId :: Lens.Lens' StudioSummary (Lude.Maybe Lude.Text)
ssStudioId = Lens.lens (studioId :: StudioSummary -> Lude.Maybe Lude.Text) (\s a -> s {studioId = a} :: StudioSummary)
{-# DEPRECATED ssStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

-- | The ID of the Virtual Private Cloud (Amazon VPC) associated with the Amazon EMR Studio.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssVPCId :: Lens.Lens' StudioSummary (Lude.Maybe Lude.Text)
ssVPCId = Lens.lens (vpcId :: StudioSummary -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: StudioSummary)
{-# DEPRECATED ssVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The unique access URL of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssURL :: Lens.Lens' StudioSummary (Lude.Maybe Lude.Text)
ssURL = Lens.lens (url :: StudioSummary -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: StudioSummary)
{-# DEPRECATED ssURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The name of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' StudioSummary (Lude.Maybe Lude.Text)
ssName = Lens.lens (name :: StudioSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StudioSummary)
{-# DEPRECATED ssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The detailed description of the EMR Studio.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssDescription :: Lens.Lens' StudioSummary (Lude.Maybe Lude.Text)
ssDescription = Lens.lens (description :: StudioSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StudioSummary)
{-# DEPRECATED ssDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON StudioSummary where
  parseJSON =
    Lude.withObject
      "StudioSummary"
      ( \x ->
          StudioSummary'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "StudioId")
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Description")
      )
