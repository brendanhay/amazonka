{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.PlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.PlatformApplication
  ( PlatformApplication (..),

    -- * Smart constructor
    mkPlatformApplication,

    -- * Lenses
    paPlatformApplicationARN,
    paAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Platform application object.
--
-- /See:/ 'mkPlatformApplication' smart constructor.
data PlatformApplication = PlatformApplication'
  { -- | PlatformApplicationArn for platform application object.
    platformApplicationARN :: Lude.Maybe Lude.Text,
    -- | Attributes for platform application object.
    attributes :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlatformApplication' with the minimum fields required to make a request.
--
-- * 'platformApplicationARN' - PlatformApplicationArn for platform application object.
-- * 'attributes' - Attributes for platform application object.
mkPlatformApplication ::
  PlatformApplication
mkPlatformApplication =
  PlatformApplication'
    { platformApplicationARN = Lude.Nothing,
      attributes = Lude.Nothing
    }

-- | PlatformApplicationArn for platform application object.
--
-- /Note:/ Consider using 'platformApplicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paPlatformApplicationARN :: Lens.Lens' PlatformApplication (Lude.Maybe Lude.Text)
paPlatformApplicationARN = Lens.lens (platformApplicationARN :: PlatformApplication -> Lude.Maybe Lude.Text) (\s a -> s {platformApplicationARN = a} :: PlatformApplication)
{-# DEPRECATED paPlatformApplicationARN "Use generic-lens or generic-optics with 'platformApplicationARN' instead." #-}

-- | Attributes for platform application object.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
paAttributes :: Lens.Lens' PlatformApplication (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
paAttributes = Lens.lens (attributes :: PlatformApplication -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: PlatformApplication)
{-# DEPRECATED paAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromXML PlatformApplication where
  parseXML x =
    PlatformApplication'
      Lude.<$> (x Lude..@? "PlatformApplicationArn")
      Lude.<*> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
               )
