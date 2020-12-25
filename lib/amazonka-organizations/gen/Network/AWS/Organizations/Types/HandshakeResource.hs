{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeResource
  ( HandshakeResource (..),

    -- * Smart constructor
    mkHandshakeResource,

    -- * Lenses
    hrResources,
    hrType,
    hrValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.HandshakeResourceType as Types
import qualified Network.AWS.Organizations.Types.Value as Types
import qualified Network.AWS.Prelude as Core

-- | Contains additional data that is needed to process a handshake.
--
-- /See:/ 'mkHandshakeResource' smart constructor.
data HandshakeResource = HandshakeResource'
  { -- | When needed, contains an additional array of @HandshakeResource@ objects.
    resources :: Core.Maybe [HandshakeResource],
    -- | The type of information being passed, specifying how the value is to be interpreted by the other party:
    --
    --
    --     * @ACCOUNT@ - Specifies an AWS account ID number.
    --
    --
    --     * @ORGANIZATION@ - Specifies an organization ID number.
    --
    --
    --     * @EMAIL@ - Specifies the email address that is associated with the account that receives the handshake.
    --
    --
    --     * @OWNER_EMAIL@ - Specifies the email address associated with the management account. Included as information about an organization.
    --
    --
    --     * @OWNER_NAME@ - Specifies the name associated with the management account. Included as information about an organization.
    --
    --
    --     * @NOTES@ - Additional text provided by the handshake initiator and intended for the recipient to read.
    type' :: Core.Maybe Types.HandshakeResourceType,
    -- | The information that is passed to the other party in the handshake. The format of the value string must match the requirements of the specified type.
    value :: Core.Maybe Types.Value
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HandshakeResource' value with any optional fields omitted.
mkHandshakeResource ::
  HandshakeResource
mkHandshakeResource =
  HandshakeResource'
    { resources = Core.Nothing,
      type' = Core.Nothing,
      value = Core.Nothing
    }

-- | When needed, contains an additional array of @HandshakeResource@ objects.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrResources :: Lens.Lens' HandshakeResource (Core.Maybe [HandshakeResource])
hrResources = Lens.field @"resources"
{-# DEPRECATED hrResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The type of information being passed, specifying how the value is to be interpreted by the other party:
--
--
--     * @ACCOUNT@ - Specifies an AWS account ID number.
--
--
--     * @ORGANIZATION@ - Specifies an organization ID number.
--
--
--     * @EMAIL@ - Specifies the email address that is associated with the account that receives the handshake.
--
--
--     * @OWNER_EMAIL@ - Specifies the email address associated with the management account. Included as information about an organization.
--
--
--     * @OWNER_NAME@ - Specifies the name associated with the management account. Included as information about an organization.
--
--
--     * @NOTES@ - Additional text provided by the handshake initiator and intended for the recipient to read.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrType :: Lens.Lens' HandshakeResource (Core.Maybe Types.HandshakeResourceType)
hrType = Lens.field @"type'"
{-# DEPRECATED hrType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The information that is passed to the other party in the handshake. The format of the value string must match the requirements of the specified type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrValue :: Lens.Lens' HandshakeResource (Core.Maybe Types.Value)
hrValue = Lens.field @"value"
{-# DEPRECATED hrValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON HandshakeResource where
  parseJSON =
    Core.withObject "HandshakeResource" Core.$
      \x ->
        HandshakeResource'
          Core.<$> (x Core..:? "Resources")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "Value")
