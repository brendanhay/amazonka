{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BlobAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BlobAttributeValue
  ( BlobAttributeValue (..),

    -- * Smart constructor
    mkBlobAttributeValue,

    -- * Lenses
    bavValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkBlobAttributeValue' smart constructor.
newtype BlobAttributeValue = BlobAttributeValue'
  { value :: Core.Maybe Core.Base64
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BlobAttributeValue' value with any optional fields omitted.
mkBlobAttributeValue ::
  BlobAttributeValue
mkBlobAttributeValue = BlobAttributeValue' {value = Core.Nothing}

-- | Undocumented field.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bavValue :: Lens.Lens' BlobAttributeValue (Core.Maybe Core.Base64)
bavValue = Lens.field @"value"
{-# DEPRECATED bavValue "Use generic-lens or generic-optics with 'value' instead." #-}
