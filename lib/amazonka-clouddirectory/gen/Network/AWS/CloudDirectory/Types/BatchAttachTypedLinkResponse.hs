{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse
  ( BatchAttachTypedLinkResponse (..),

    -- * Smart constructor
    mkBatchAttachTypedLinkResponse,

    -- * Lenses
    batlTypedLinkSpecifier,
  )
where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'AttachTypedLink' response operation.
--
-- /See:/ 'mkBatchAttachTypedLinkResponse' smart constructor.
newtype BatchAttachTypedLinkResponse = BatchAttachTypedLinkResponse'
  { typedLinkSpecifier ::
      Lude.Maybe TypedLinkSpecifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchAttachTypedLinkResponse' with the minimum fields required to make a request.
--
-- * 'typedLinkSpecifier' - Returns a typed link specifier as output.
mkBatchAttachTypedLinkResponse ::
  BatchAttachTypedLinkResponse
mkBatchAttachTypedLinkResponse =
  BatchAttachTypedLinkResponse' {typedLinkSpecifier = Lude.Nothing}

-- | Returns a typed link specifier as output.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlTypedLinkSpecifier :: Lens.Lens' BatchAttachTypedLinkResponse (Lude.Maybe TypedLinkSpecifier)
batlTypedLinkSpecifier = Lens.lens (typedLinkSpecifier :: BatchAttachTypedLinkResponse -> Lude.Maybe TypedLinkSpecifier) (\s a -> s {typedLinkSpecifier = a} :: BatchAttachTypedLinkResponse)
{-# DEPRECATED batlTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

instance Lude.FromJSON BatchAttachTypedLinkResponse where
  parseJSON =
    Lude.withObject
      "BatchAttachTypedLinkResponse"
      ( \x ->
          BatchAttachTypedLinkResponse'
            Lude.<$> (x Lude..:? "TypedLinkSpecifier")
      )
