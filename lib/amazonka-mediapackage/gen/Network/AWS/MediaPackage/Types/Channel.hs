{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Channel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Channel
  ( Channel (..),

    -- * Smart constructor
    mkChannel,

    -- * Lenses
    cIngressAccessLogs,
    cHlsIngest,
    cARN,
    cId,
    cDescription,
    cEgressAccessLogs,
    cTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.EgressAccessLogs
import Network.AWS.MediaPackage.Types.HlsIngest
import Network.AWS.MediaPackage.Types.IngressAccessLogs
import qualified Network.AWS.Prelude as Lude

-- | A Channel resource configuration.
--
-- /See:/ 'mkChannel' smart constructor.
data Channel = Channel'
  { ingressAccessLogs :: Lude.Maybe IngressAccessLogs,
    hlsIngest :: Lude.Maybe HlsIngest,
    -- | The Amazon Resource Name (ARN) assigned to the Channel.
    arn :: Lude.Maybe Lude.Text,
    -- | The ID of the Channel.
    id :: Lude.Maybe Lude.Text,
    -- | A short text description of the Channel.
    description :: Lude.Maybe Lude.Text,
    egressAccessLogs :: Lude.Maybe EgressAccessLogs,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Channel' with the minimum fields required to make a request.
--
-- * 'ingressAccessLogs' -
-- * 'hlsIngest' -
-- * 'arn' - The Amazon Resource Name (ARN) assigned to the Channel.
-- * 'id' - The ID of the Channel.
-- * 'description' - A short text description of the Channel.
-- * 'egressAccessLogs' -
-- * 'tags' -
mkChannel ::
  Channel
mkChannel =
  Channel'
    { ingressAccessLogs = Lude.Nothing,
      hlsIngest = Lude.Nothing,
      arn = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing,
      egressAccessLogs = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'ingressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIngressAccessLogs :: Lens.Lens' Channel (Lude.Maybe IngressAccessLogs)
cIngressAccessLogs = Lens.lens (ingressAccessLogs :: Channel -> Lude.Maybe IngressAccessLogs) (\s a -> s {ingressAccessLogs = a} :: Channel)
{-# DEPRECATED cIngressAccessLogs "Use generic-lens or generic-optics with 'ingressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'hlsIngest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHlsIngest :: Lens.Lens' Channel (Lude.Maybe HlsIngest)
cHlsIngest = Lens.lens (hlsIngest :: Channel -> Lude.Maybe HlsIngest) (\s a -> s {hlsIngest = a} :: Channel)
{-# DEPRECATED cHlsIngest "Use generic-lens or generic-optics with 'hlsIngest' instead." #-}

-- | The Amazon Resource Name (ARN) assigned to the Channel.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cARN :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cARN = Lens.lens (arn :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Channel)
{-# DEPRECATED cARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The ID of the Channel.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cId :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cId = Lens.lens (id :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Channel)
{-# DEPRECATED cId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A short text description of the Channel.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDescription :: Lens.Lens' Channel (Lude.Maybe Lude.Text)
cDescription = Lens.lens (description :: Channel -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Channel)
{-# DEPRECATED cDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'egressAccessLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cEgressAccessLogs :: Lens.Lens' Channel (Lude.Maybe EgressAccessLogs)
cEgressAccessLogs = Lens.lens (egressAccessLogs :: Channel -> Lude.Maybe EgressAccessLogs) (\s a -> s {egressAccessLogs = a} :: Channel)
{-# DEPRECATED cEgressAccessLogs "Use generic-lens or generic-optics with 'egressAccessLogs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' Channel (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cTags = Lens.lens (tags :: Channel -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: Channel)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Channel where
  parseJSON =
    Lude.withObject
      "Channel"
      ( \x ->
          Channel'
            Lude.<$> (x Lude..:? "ingressAccessLogs")
            Lude.<*> (x Lude..:? "hlsIngest")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "egressAccessLogs")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
