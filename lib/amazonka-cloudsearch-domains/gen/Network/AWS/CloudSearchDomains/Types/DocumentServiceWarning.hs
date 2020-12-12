{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.DocumentServiceWarning
  ( DocumentServiceWarning (..),

    -- * Smart constructor
    mkDocumentServiceWarning,

    -- * Lenses
    dswMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A warning returned by the document service when an issue is discovered while processing an upload request.
--
-- /See:/ 'mkDocumentServiceWarning' smart constructor.
newtype DocumentServiceWarning = DocumentServiceWarning'
  { message ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentServiceWarning' with the minimum fields required to make a request.
--
-- * 'message' - The description for a warning returned by the document service.
mkDocumentServiceWarning ::
  DocumentServiceWarning
mkDocumentServiceWarning =
  DocumentServiceWarning' {message = Lude.Nothing}

-- | The description for a warning returned by the document service.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dswMessage :: Lens.Lens' DocumentServiceWarning (Lude.Maybe Lude.Text)
dswMessage = Lens.lens (message :: DocumentServiceWarning -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DocumentServiceWarning)
{-# DEPRECATED dswMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON DocumentServiceWarning where
  parseJSON =
    Lude.withObject
      "DocumentServiceWarning"
      (\x -> DocumentServiceWarning' Lude.<$> (x Lude..:? "message"))
