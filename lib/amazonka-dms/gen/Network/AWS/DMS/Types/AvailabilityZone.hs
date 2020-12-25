{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AvailabilityZone
  ( AvailabilityZone (..),

    -- * Smart constructor
    mkAvailabilityZone,

    -- * Lenses
    azName,
  )
where

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The name of an Availability Zone for use during database migration. @AvailabilityZone@ is an optional parameter to the <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationInstance.html @CreateReplicationInstance@ > operation, and it’s value relates to the AWS Region of an endpoint. For example, the availability zone of an endpoint in the us-east-1 region might be us-east-1a, us-east-1b, us-east-1c, or us-east-1d.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { -- | The name of the Availability Zone.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZone' value with any optional fields omitted.
mkAvailabilityZone ::
  AvailabilityZone
mkAvailabilityZone = AvailabilityZone' {name = Core.Nothing}

-- | The name of the Availability Zone.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azName :: Lens.Lens' AvailabilityZone (Core.Maybe Types.String)
azName = Lens.field @"name"
{-# DEPRECATED azName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON AvailabilityZone where
  parseJSON =
    Core.withObject "AvailabilityZone" Core.$
      \x -> AvailabilityZone' Core.<$> (x Core..:? "Name")
