{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupCertificateAuthorityProperties
  ( GroupCertificateAuthorityProperties (..),

    -- * Smart constructor
    mkGroupCertificateAuthorityProperties,

    -- * Lenses
    gcapGroupCertificateAuthorityARN,
    gcapGroupCertificateAuthorityId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a certificate authority for a group.
--
-- /See:/ 'mkGroupCertificateAuthorityProperties' smart constructor.
data GroupCertificateAuthorityProperties = GroupCertificateAuthorityProperties'
  { groupCertificateAuthorityARN ::
      Lude.Maybe
        Lude.Text,
    groupCertificateAuthorityId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupCertificateAuthorityProperties' with the minimum fields required to make a request.
--
-- * 'groupCertificateAuthorityARN' - The ARN of the certificate authority for the group.
-- * 'groupCertificateAuthorityId' - The ID of the certificate authority for the group.
mkGroupCertificateAuthorityProperties ::
  GroupCertificateAuthorityProperties
mkGroupCertificateAuthorityProperties =
  GroupCertificateAuthorityProperties'
    { groupCertificateAuthorityARN =
        Lude.Nothing,
      groupCertificateAuthorityId = Lude.Nothing
    }

-- | The ARN of the certificate authority for the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcapGroupCertificateAuthorityARN :: Lens.Lens' GroupCertificateAuthorityProperties (Lude.Maybe Lude.Text)
gcapGroupCertificateAuthorityARN = Lens.lens (groupCertificateAuthorityARN :: GroupCertificateAuthorityProperties -> Lude.Maybe Lude.Text) (\s a -> s {groupCertificateAuthorityARN = a} :: GroupCertificateAuthorityProperties)
{-# DEPRECATED gcapGroupCertificateAuthorityARN "Use generic-lens or generic-optics with 'groupCertificateAuthorityARN' instead." #-}

-- | The ID of the certificate authority for the group.
--
-- /Note:/ Consider using 'groupCertificateAuthorityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcapGroupCertificateAuthorityId :: Lens.Lens' GroupCertificateAuthorityProperties (Lude.Maybe Lude.Text)
gcapGroupCertificateAuthorityId = Lens.lens (groupCertificateAuthorityId :: GroupCertificateAuthorityProperties -> Lude.Maybe Lude.Text) (\s a -> s {groupCertificateAuthorityId = a} :: GroupCertificateAuthorityProperties)
{-# DEPRECATED gcapGroupCertificateAuthorityId "Use generic-lens or generic-optics with 'groupCertificateAuthorityId' instead." #-}

instance Lude.FromJSON GroupCertificateAuthorityProperties where
  parseJSON =
    Lude.withObject
      "GroupCertificateAuthorityProperties"
      ( \x ->
          GroupCertificateAuthorityProperties'
            Lude.<$> (x Lude..:? "GroupCertificateAuthorityArn")
            Lude.<*> (x Lude..:? "GroupCertificateAuthorityId")
      )
