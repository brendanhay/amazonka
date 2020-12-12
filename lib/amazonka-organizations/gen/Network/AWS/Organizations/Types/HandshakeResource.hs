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
    hrValue,
    hrResources,
    hrType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.HandshakeResourceType
import qualified Network.AWS.Prelude as Lude

-- | Contains additional data that is needed to process a handshake.
--
-- /See:/ 'mkHandshakeResource' smart constructor.
data HandshakeResource = HandshakeResource'
  { value ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    resources :: Lude.Maybe [HandshakeResource],
    type' :: Lude.Maybe HandshakeResourceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HandshakeResource' with the minimum fields required to make a request.
--
-- * 'resources' - When needed, contains an additional array of @HandshakeResource@ objects.
-- * 'type'' - The type of information being passed, specifying how the value is to be interpreted by the other party:
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
-- * 'value' - The information that is passed to the other party in the handshake. The format of the value string must match the requirements of the specified type.
mkHandshakeResource ::
  HandshakeResource
mkHandshakeResource =
  HandshakeResource'
    { value = Lude.Nothing,
      resources = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The information that is passed to the other party in the handshake. The format of the value string must match the requirements of the specified type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrValue :: Lens.Lens' HandshakeResource (Lude.Maybe (Lude.Sensitive Lude.Text))
hrValue = Lens.lens (value :: HandshakeResource -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {value = a} :: HandshakeResource)
{-# DEPRECATED hrValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | When needed, contains an additional array of @HandshakeResource@ objects.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hrResources :: Lens.Lens' HandshakeResource (Lude.Maybe [HandshakeResource])
hrResources = Lens.lens (resources :: HandshakeResource -> Lude.Maybe [HandshakeResource]) (\s a -> s {resources = a} :: HandshakeResource)
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
hrType :: Lens.Lens' HandshakeResource (Lude.Maybe HandshakeResourceType)
hrType = Lens.lens (type' :: HandshakeResource -> Lude.Maybe HandshakeResourceType) (\s a -> s {type' = a} :: HandshakeResource)
{-# DEPRECATED hrType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON HandshakeResource where
  parseJSON =
    Lude.withObject
      "HandshakeResource"
      ( \x ->
          HandshakeResource'
            Lude.<$> (x Lude..:? "Value")
            Lude.<*> (x Lude..:? "Resources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Type")
      )
