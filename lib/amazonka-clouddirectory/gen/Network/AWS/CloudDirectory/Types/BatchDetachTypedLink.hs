{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchDetachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchDetachTypedLink
  ( BatchDetachTypedLink (..),

    -- * Smart constructor
    mkBatchDetachTypedLink,

    -- * Lenses
    bdtlTypedLinkSpecifier,
  )
where

import qualified Network.AWS.CloudDirectory.Types.TypedLinkSpecifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detaches a typed link from a specified source and target object inside a 'BatchRead' operation. For more information, see 'DetachTypedLink' and 'BatchReadRequest$Operations' .
--
-- /See:/ 'mkBatchDetachTypedLink' smart constructor.
newtype BatchDetachTypedLink = BatchDetachTypedLink'
  { -- | Used to accept a typed link specifier as input.
    typedLinkSpecifier :: Types.TypedLinkSpecifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'BatchDetachTypedLink' value with any optional fields omitted.
mkBatchDetachTypedLink ::
  -- | 'typedLinkSpecifier'
  Types.TypedLinkSpecifier ->
  BatchDetachTypedLink
mkBatchDetachTypedLink typedLinkSpecifier =
  BatchDetachTypedLink' {typedLinkSpecifier}

-- | Used to accept a typed link specifier as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdtlTypedLinkSpecifier :: Lens.Lens' BatchDetachTypedLink Types.TypedLinkSpecifier
bdtlTypedLinkSpecifier = Lens.field @"typedLinkSpecifier"
{-# DEPRECATED bdtlTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

instance Core.FromJSON BatchDetachTypedLink where
  toJSON BatchDetachTypedLink {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TypedLinkSpecifier" Core..= typedLinkSpecifier)]
      )
