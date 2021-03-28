{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.Source
  ( Source (..)
  -- * Smart constructor
  , mkSource
  -- * Lenses
  , sS3Location
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.S3Location as Types

-- | Contains the location of a validation script.
--
-- /See:/ 'mkSource' smart constructor.
newtype Source = Source'
  { s3Location :: Core.Maybe Types.S3Location
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Source' value with any optional fields omitted.
mkSource
    :: Source
mkSource = Source'{s3Location = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sS3Location :: Lens.Lens' Source (Core.Maybe Types.S3Location)
sS3Location = Lens.field @"s3Location"
{-# INLINEABLE sS3Location #-}
{-# DEPRECATED s3Location "Use generic-lens or generic-optics with 's3Location' instead"  #-}

instance Core.FromJSON Source where
        toJSON Source{..}
          = Core.object
              (Core.catMaybes [("s3Location" Core..=) Core.<$> s3Location])

instance Core.FromJSON Source where
        parseJSON
          = Core.withObject "Source" Core.$
              \ x -> Source' Core.<$> (x Core..:? "s3Location")
