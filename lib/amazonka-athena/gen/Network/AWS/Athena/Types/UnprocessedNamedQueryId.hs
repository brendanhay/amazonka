{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.UnprocessedNamedQueryId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.UnprocessedNamedQueryId
  ( UnprocessedNamedQueryId (..)
  -- * Smart constructor
  , mkUnprocessedNamedQueryId
  -- * Lenses
  , unqiErrorCode
  , unqiErrorMessage
  , unqiNamedQueryId
  ) where

import qualified Network.AWS.Athena.Types.ErrorCode as Types
import qualified Network.AWS.Athena.Types.ErrorMessage as Types
import qualified Network.AWS.Athena.Types.NamedQueryId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a named query ID that could not be processed.
--
-- /See:/ 'mkUnprocessedNamedQueryId' smart constructor.
data UnprocessedNamedQueryId = UnprocessedNamedQueryId'
  { errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The error code returned when the processing request for the named query failed, if applicable.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ The error message returned when the processing request for the named query failed, if applicable.
  , namedQueryId :: Core.Maybe Types.NamedQueryId
    -- ^ The unique identifier of the named query.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedNamedQueryId' value with any optional fields omitted.
mkUnprocessedNamedQueryId
    :: UnprocessedNamedQueryId
mkUnprocessedNamedQueryId
  = UnprocessedNamedQueryId'{errorCode = Core.Nothing,
                             errorMessage = Core.Nothing, namedQueryId = Core.Nothing}

-- | The error code returned when the processing request for the named query failed, if applicable.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unqiErrorCode :: Lens.Lens' UnprocessedNamedQueryId (Core.Maybe Types.ErrorCode)
unqiErrorCode = Lens.field @"errorCode"
{-# INLINEABLE unqiErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message returned when the processing request for the named query failed, if applicable.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unqiErrorMessage :: Lens.Lens' UnprocessedNamedQueryId (Core.Maybe Types.ErrorMessage)
unqiErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE unqiErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The unique identifier of the named query.
--
-- /Note:/ Consider using 'namedQueryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
unqiNamedQueryId :: Lens.Lens' UnprocessedNamedQueryId (Core.Maybe Types.NamedQueryId)
unqiNamedQueryId = Lens.field @"namedQueryId"
{-# INLINEABLE unqiNamedQueryId #-}
{-# DEPRECATED namedQueryId "Use generic-lens or generic-optics with 'namedQueryId' instead"  #-}

instance Core.FromJSON UnprocessedNamedQueryId where
        parseJSON
          = Core.withObject "UnprocessedNamedQueryId" Core.$
              \ x ->
                UnprocessedNamedQueryId' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "NamedQueryId"
