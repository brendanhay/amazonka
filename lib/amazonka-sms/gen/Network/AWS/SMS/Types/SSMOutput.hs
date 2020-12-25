{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.SSMOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.SSMOutput
  ( SSMOutput (..),

    -- * Smart constructor
    mkSSMOutput,

    -- * Lenses
    ssmoS3Location,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.S3Location as Types

-- | Contains the location of validation output.
--
-- /See:/ 'mkSSMOutput' smart constructor.
newtype SSMOutput = SSMOutput'
  { s3Location :: Core.Maybe Types.S3Location
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SSMOutput' value with any optional fields omitted.
mkSSMOutput ::
  SSMOutput
mkSSMOutput = SSMOutput' {s3Location = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssmoS3Location :: Lens.Lens' SSMOutput (Core.Maybe Types.S3Location)
ssmoS3Location = Lens.field @"s3Location"
{-# DEPRECATED ssmoS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

instance Core.FromJSON SSMOutput where
  parseJSON =
    Core.withObject "SSMOutput" Core.$
      \x -> SSMOutput' Core.<$> (x Core..:? "s3Location")
