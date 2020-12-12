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
    slName,
    slCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A code and name pair that represents the severity level of a support case. The available values depend on the support plan for the account. For more information, see <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity> in the /AWS Support User Guide/ .
--
-- /See:/ 'mkSeverityLevel' smart constructor.
data SeverityLevel = SeverityLevel'
  { name :: Lude.Maybe Lude.Text,
    code :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SeverityLevel' with the minimum fields required to make a request.
--
-- * 'code' - The code for case severity level.
--
-- Valid values: @low@ | @normal@ | @high@ | @urgent@ | @critical@
-- * 'name' - The name of the severity level that corresponds to the severity level code.
--
-- For more information, see <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity> in the /AWS Support User Guide/ .
mkSeverityLevel ::
  SeverityLevel
mkSeverityLevel =
  SeverityLevel' {name = Lude.Nothing, code = Lude.Nothing}

-- | The name of the severity level that corresponds to the severity level code.
--
-- For more information, see <https://docs.aws.amazon.com/awssupport/latest/user/case-management.html#choosing-severity Choosing a severity> in the /AWS Support User Guide/ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slName :: Lens.Lens' SeverityLevel (Lude.Maybe Lude.Text)
slName = Lens.lens (name :: SeverityLevel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SeverityLevel)
{-# DEPRECATED slName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The code for case severity level.
--
-- Valid values: @low@ | @normal@ | @high@ | @urgent@ | @critical@
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slCode :: Lens.Lens' SeverityLevel (Lude.Maybe Lude.Text)
slCode = Lens.lens (code :: SeverityLevel -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: SeverityLevel)
{-# DEPRECATED slCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromJSON SeverityLevel where
  parseJSON =
    Lude.withObject
      "SeverityLevel"
      ( \x ->
          SeverityLevel'
            Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "code")
      )
