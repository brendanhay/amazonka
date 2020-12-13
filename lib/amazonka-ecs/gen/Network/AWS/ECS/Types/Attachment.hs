{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Attachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Attachment
  ( Attachment (..),

    -- * Smart constructor
    mkAttachment,

    -- * Lenses
    aStatus,
    aDetails,
    aId,
    aType,
  )
where

import Network.AWS.ECS.Types.KeyValuePair
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a container instance or task attachment.
--
-- /See:/ 'mkAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
    status :: Lude.Maybe Lude.Text,
    -- | Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
    details :: Lude.Maybe [KeyValuePair],
    -- | The unique identifier for the attachment.
    id :: Lude.Maybe Lude.Text,
    -- | The type of the attachment, such as @ElasticNetworkInterface@ .
    type' :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Attachment' with the minimum fields required to make a request.
--
-- * 'status' - The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
-- * 'details' - Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
-- * 'id' - The unique identifier for the attachment.
-- * 'type'' - The type of the attachment, such as @ElasticNetworkInterface@ .
mkAttachment ::
  Attachment
mkAttachment =
  Attachment'
    { status = Lude.Nothing,
      details = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The status of the attachment. Valid values are @PRECREATED@ , @CREATED@ , @ATTACHING@ , @ATTACHED@ , @DETACHING@ , @DETACHED@ , and @DELETED@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStatus :: Lens.Lens' Attachment (Lude.Maybe Lude.Text)
aStatus = Lens.lens (status :: Attachment -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Attachment)
{-# DEPRECATED aStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Details of the attachment. For elastic network interfaces, this includes the network interface ID, the MAC address, the subnet ID, and the private IPv4 address.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDetails :: Lens.Lens' Attachment (Lude.Maybe [KeyValuePair])
aDetails = Lens.lens (details :: Attachment -> Lude.Maybe [KeyValuePair]) (\s a -> s {details = a} :: Attachment)
{-# DEPRECATED aDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The unique identifier for the attachment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aId :: Lens.Lens' Attachment (Lude.Maybe Lude.Text)
aId = Lens.lens (id :: Attachment -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Attachment)
{-# DEPRECATED aId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the attachment, such as @ElasticNetworkInterface@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Attachment (Lude.Maybe Lude.Text)
aType = Lens.lens (type' :: Attachment -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Attachment)
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Attachment where
  parseJSON =
    Lude.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "details" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "type")
      )
