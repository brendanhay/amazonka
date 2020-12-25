{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.SeverityLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.SeverityLevel
  ( SeverityLevel (..),

    -- * Smart constructor
    mkSeverityLevel,

    -- * Lenses
    slCode,
    slName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.SeverityLevelCode as Types
import qualified Network.AWS.Support.Types.SeverityLevelName as Types

-- | A code and name pair that represents the severity level of a support case. The available values depend on the support plan for the account. For more information, see <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity> in the /AWS Support User Guide/ .
--
-- /See:/ 'mkSeverityLevel' smart constructor.
data SeverityLevel = SeverityLevel'
  { -- | The code for case severity level.
    --
    -- Valid values: @low@ | @normal@ | @high@ | @urgent@ | @critical@
    code :: Core.Maybe Types.SeverityLevelCode,
    -- | The name of the severity level that corresponds to the severity level code.
    --
    -- For more information, see <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity> in the /AWS Support User Guide/ .
    name :: Core.Maybe Types.SeverityLevelName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SeverityLevel' value with any optional fields omitted.
mkSeverityLevel ::
  SeverityLevel
mkSeverityLevel =
  SeverityLevel' {code = Core.Nothing, name = Core.Nothing}

-- | The code for case severity level.
--
-- Valid values: @low@ | @normal@ | @high@ | @urgent@ | @critical@
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slCode :: Lens.Lens' SeverityLevel (Core.Maybe Types.SeverityLevelCode)
slCode = Lens.field @"code"
{-# DEPRECATED slCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The name of the severity level that corresponds to the severity level code.
--
-- For more information, see <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity> in the /AWS Support User Guide/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slName :: Lens.Lens' SeverityLevel (Core.Maybe Types.SeverityLevelName)
slName = Lens.field @"name"
{-# DEPRECATED slName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON SeverityLevel where
  parseJSON =
    Core.withObject "SeverityLevel" Core.$
      \x ->
        SeverityLevel'
          Core.<$> (x Core..:? "code") Core.<*> (x Core..:? "name")
