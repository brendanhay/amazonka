{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DataLakePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DataLakePrincipal
  ( DataLakePrincipal (..),

    -- * Smart constructor
    mkDataLakePrincipal,

    -- * Lenses
    dlpDataLakePrincipalIdentifier,
  )
where

import qualified Network.AWS.Glue.Types.DataLakePrincipalString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The AWS Lake Formation principal.
--
-- /See:/ 'mkDataLakePrincipal' smart constructor.
newtype DataLakePrincipal = DataLakePrincipal'
  { -- | An identifier for the AWS Lake Formation principal.
    dataLakePrincipalIdentifier :: Core.Maybe Types.DataLakePrincipalString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DataLakePrincipal' value with any optional fields omitted.
mkDataLakePrincipal ::
  DataLakePrincipal
mkDataLakePrincipal =
  DataLakePrincipal' {dataLakePrincipalIdentifier = Core.Nothing}

-- | An identifier for the AWS Lake Formation principal.
--
-- /Note:/ Consider using 'dataLakePrincipalIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlpDataLakePrincipalIdentifier :: Lens.Lens' DataLakePrincipal (Core.Maybe Types.DataLakePrincipalString)
dlpDataLakePrincipalIdentifier = Lens.field @"dataLakePrincipalIdentifier"
{-# DEPRECATED dlpDataLakePrincipalIdentifier "Use generic-lens or generic-optics with 'dataLakePrincipalIdentifier' instead." #-}

instance Core.FromJSON DataLakePrincipal where
  toJSON DataLakePrincipal {..} =
    Core.object
      ( Core.catMaybes
          [ ("DataLakePrincipalIdentifier" Core..=)
              Core.<$> dataLakePrincipalIdentifier
          ]
      )

instance Core.FromJSON DataLakePrincipal where
  parseJSON =
    Core.withObject "DataLakePrincipal" Core.$
      \x ->
        DataLakePrincipal'
          Core.<$> (x Core..:? "DataLakePrincipalIdentifier")
